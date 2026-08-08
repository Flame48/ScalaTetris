import Coord.*
import scala.compiletime.ops.boolean
import scala.collection.mutable

trait Shape {
  val position: Coord
  val offsets: Seq[Coord]

  def collidesWith(other: Shape): Boolean =
    other.offsets.exists(oo =>
      offsets.exists(o => this.position + o == other.position + oo)
    )

  def withinBounds(bounds: Rectangle): Boolean =
    offsets.forall(o => {
      bounds.minX <= (position + o).x && (position + o).x <= bounds.maxX &&
      bounds.minY <= (position + o).y && (position + o).y <= bounds.maxY
    })

  def coords(offset: Coord = Origin): Seq[Coord] =
    for o <- offsets
    yield position + o + offset
}

case class Block(position: Coord) extends Shape {
  def pos = position
  val offsets = Array(Coord.Origin)
}

sealed case class Tetromino(offsets: Seq[Coord], position: Coord = Coord.Origin)
    extends Shape {

  def rotate(n: Int = 1): Tetromino =
    new Tetromino(offsets.map(o => o.rotate(n)), position)

  def translate(offset: Coord): Tetromino =
    new Tetromino(offsets, position + offset)

}

object Tetromino {
  def T(at: Coord = Coord.Origin): Tetromino =
    Tetromino(Seq((-1, 0), (0, 0), (1, 0), (0, 1)), at)

  def I(at: Coord = Coord.Origin): Tetromino =
    Tetromino(Seq((-2, 0), (-1, 0), (0, 0), (1, 0)), at)

  def J(at: Coord = Coord.Origin): Tetromino =
    Tetromino(Seq((-1, -1), (-1, 0), (0, 0), (1, 0)), at)

  def L(at: Coord = Coord.Origin): Tetromino =
    Tetromino(Seq((1, -1), (-1, 0), (0, 0), (1, 0)), at)

  def S(at: Coord = Coord.Origin): Tetromino =
    Tetromino(Seq((0, 0), (1, 0), (-1, 1), (0, 1)), at)

  def Z(at: Coord = Coord.Origin): Tetromino =
    Tetromino(Seq((-1, 0), (0, 0), (0, 1), (1, 1)), at)

  def O(at: Coord = Coord.Origin): Tetromino =
    new Tetromino(Seq((0, 0), (1, 0), (0, 1), (1, 1)), at) {
      override def rotate(n: Int = 1): Tetromino =
        this
      override def translate(offset: Coord): Tetromino =
        O(position + offset)
    }

  def all: Seq[Coord => Tetromino] = Seq(I, O, T, S, Z, J, L)

  def random(at: Coord = Coord.Origin): Tetromino =
    all(scala.util.Random.nextInt(all.length))(at)
}

object GameBoard {
  enum Input {
    case Left
    case Right
    case Down
    case Rotate
    case HardDrop
  }
}

class GameBoard(width: Int, height: Int, pocket: Pocket) {

  val bounds: Rectangle = Rectangle(Coord.Origin, width, height)
  var board: Grid[Option[Block]] = Array.fill(width, height)(None)
  val spawnLocation: Coord = (width / 2, 2)

  var current: Option[Tetromino] = None

  var gravityTimer: Timer = new Running(8)
  var spawnTimer: Timer = new Running(12)

  val inputQueue: Queue[GameBoard.Input] = new Queue()

  // Locked "shape"
  def locked(): Shape = new Shape {
    val position: Coord = bounds.tl
    val offsets: Seq[Coord] =
      for
        x <- 0 until width
        y <- 0 until height
        if board(x)(y).isDefined
      yield (x, y)
  }

  def clearRow(row: Int) = {
    for (
      y <- row to 1 by -1;
      x <- 0 until width
    ) {
      board(x)(y) = board(x)(y - 1)
    }
    for (x <- 0 until width) {
      board(x)(0) = None
    }
  }

  def lock() = {
    current.foreach { t =>
      current = None
      t.offsets.foreach { o =>
        val p = t.position + o
        board(p.x)(p.y) = Some(Block(p))
      }
    }

    // Clear rows
    val fullRows = (0 until height).filter(y =>
      (0 until width).forall(x => {
        board(x)(y).isDefined
      })
    )

    fullRows.foreach(clearRow)
  }

  def spawn(): Tetromino =
    Tetromino.random(spawnLocation).rotate(scala.util.Random.nextInt(4))

  def process() = {
    if (inputQueue.length > 0) {
      val input = inputQueue.pop()
      inputQueue.clear()
      processInput(input)
    }
    processTimers()
  }

  def processInput(input: GameBoard.Input) = {
    current.foreach { value =>
      val moved = input match
        case GameBoard.Input.Left     => tryMove(value, _.translate((-1, 0)))
        case GameBoard.Input.Right    => tryMove(value, _.translate((1, 0)))
        case GameBoard.Input.Down     => tryMove(value, _.translate((0, 1)))
        case GameBoard.Input.Rotate   => tryMove(value, _.rotate())
        case GameBoard.Input.HardDrop =>
          Some(tryMoveTillFail(value, _.translate((0, 1))))
      moved.foreach(newShape => current = Some(newShape))
    }
  }

  def processTimers() = {
    gravityTimer = gravityTimer match
      case t: Running  => t.tick()
      case t: Paused   => t
      case t: Finished => {
        current.foreach { value =>
          tryMove(value, _.translate((0, 1))) match
            case Some(value) => current = Some(value)
            case None        => {
              lock()
              spawnTimer = spawnTimer.reset()
            }
        }

        t.reset()
      }

    spawnTimer = spawnTimer match
      case t: Running  => t.tick()
      case t: Paused   => t
      case t: Finished => {
        current = Some(current.getOrElse(spawn()))
        t.reset()
      }

  }

  // Returns the new shape if it doesn't collide with anything
  private def tryMove[T <: Shape](shape: T, mover: (T) => T): Option[T] = {
    val newShape = mover(shape)
    if (!newShape.withinBounds(bounds)) return None
    if (newShape.collidesWith(locked())) return None
    return Some(newShape)
  }

  private def tryMoveTillFail[T <: Shape](shape: T, mover: (T) => T): T =
    tryMove(shape, mover) match
      case Some(value) => tryMoveTillFail(value, mover)
      case None        => shape

  def rendered(at: Coord = Origin): Drawable = {
    val bW = Asset.Block.bounds.w
    val bH = Asset.Block.bounds.h

    val boundsAsset = OutlinedRectangle(
      Rectangle(at, bW * width + 2, bH * height + 2)
    )

    def cellToPixel(cell: Coord): Coord =
      at + (1, 1) + (cell.x * bW, cell.y * bH)

    val lockedDrawable = Drawable.compose(
      locked().coords().map(p => Asset.Block.translate(cellToPixel(p)))
    )

    val ghostDrawable = Drawable.compose(
      current match
        case Some(value) =>
          tryMoveTillFail(value, _.translate((0, 1)))
            .coords()
            .map(p => Asset.GhostBlock.translate(cellToPixel(p)))
        case None => List.empty[Asset]
    )

    val currentDrawable = Drawable.compose(
      current match
        case Some(value) =>
          value.coords().map(p => Asset.Block.translate(cellToPixel(p)))
        case None => List.empty[Asset]
    )

    return Drawable.compose(
      List(
        boundsAsset,
        lockedDrawable,
        ghostDrawable,
        currentDrawable
      )
    )
  }

}

class Pocket(at: Coord) {
  val width: Int = 4
  val height: Int = 4

  val bounds = Rectangle(at, width, height)

  var content: Option[Tetromino] = None

  def full: Boolean = content.isDefined

  def empty(): Option[Tetromino] = {
    swap(None)
  }

  def swap(w: Option[Tetromino]): Option[Tetromino] = {
    val p = content
    content = w
    return p
  }

}
