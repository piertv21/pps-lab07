package ex2

type Position = (Int, Int)
enum Direction:
  case North, East, South, West
  def turnRight: Direction = this match
    case Direction.North => Direction.East
    case Direction.East => Direction.South
    case Direction.South => Direction.West
    case Direction.West => Direction.North

  def turnLeft: Direction = this match
    case Direction.North => Direction.West
    case Direction.West => Direction.South
    case Direction.South => Direction.East
    case Direction.East => Direction.North

trait Robot:
  def position: Position
  def direction: Direction
  def turn(dir: Direction): Unit
  def act(): Unit

class SimpleRobot(var position: Position, var direction: Direction) extends Robot:
  def turn(dir: Direction): Unit = direction = dir
  def act(): Unit = position = direction match
    case Direction.North => (position._1, position._2 + 1)
    case Direction.East => (position._1 + 1, position._2)
    case Direction.South => (position._1, position._2 - 1)
    case Direction.West => (position._1 - 1, position._2)

  override def toString: String = s"robot at $position facing $direction"

class DumbRobot(val robot: Robot) extends Robot:
  export robot.{position, direction, act}
  override def turn(dir: Direction): Unit = {}
  override def toString: String = s"${robot.toString} (Dump)"

class LoggingRobot(val robot: Robot) extends Robot:
  export robot.{position, direction, turn}
  override def act(): Unit =
    robot.act()
    println(robot.toString)

class RobotWithBattery(val robot: Robot) extends Robot:
  export robot.{position, direction}
  private var battery: Int = 100
  override def turn(dir: Direction): Unit =
    if battery > 0 then
      robot.turn(dir)
      battery -= 1
      println(toString)
  override def act(): Unit =
    if battery > 0 then
      robot.act()
      battery -= 1
      println(toString)
  override def toString: String = s"${robot.toString} (Battery: $battery%)"

class RobotCanFail(val robot: Robot, val probabilityCanFail: Double) extends Robot:
  export robot.{position, direction}
  override def turn(dir: Direction): Unit =
    if scala.util.Random.nextDouble() > probabilityCanFail then
      robot.turn(dir)
      println(toString)
    else
      println("Robot failed to turn")
  override def act(): Unit =
    if scala.util.Random.nextDouble() > probabilityCanFail then
      robot.act()
      println(toString)
    else
      println("Robot failed to act")
  override def toString: String = s"${robot.toString} (Can fail)"

class RobotRepeated(val robot: Robot, val numberOfTimes: Int) extends Robot:
  export robot.{position, direction}
  override def turn(dir: Direction): Unit =
    for _ <- 1 to numberOfTimes do
      robot.turn(dir)
      println(toString)
  override def act(): Unit =
    for _ <- 1 to numberOfTimes do
      robot.act()
      println(toString)
  override def toString: String = s"${robot.toString} (Repeated $numberOfTimes times)"

@main def testRobot(): Unit =
  val robot = RobotRepeated(SimpleRobot((0, 0), Direction.North), 2)
  robot.act() // robot at (0, 1) facing North
  robot.turn(robot.direction.turnRight) // robot at (0, 1) facing East
  robot.act() // robot at (1, 1) facing East
  robot.act() // robot at (2, 1) facing East