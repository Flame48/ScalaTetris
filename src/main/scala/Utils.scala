type Coord = (Int, Int)
object Coord {
  def Origin: Coord = (0, 0)
  extension (coord: Coord) {
    def x = coord(0)
    def y = coord(1)
    def rotate(): Coord = (-coord.y, coord.x)
    def rotate(n: Int): Coord = {
      val num = ((n % 4) + 4) % 4
      if (num == 0) return coord
      (1 to num).foldLeft(coord)((p, _) => p.rotate())
    }
    def +(other: Coord): Coord = (coord.x + other.x, coord.y + other.y)
    def -(other: Coord): Coord = (coord.x - other.x, coord.y - other.y)
  }
}
import Coord.*
import scala.collection.mutable.ListBuffer

type Grid[T] = Array[Array[T]]

class Queue[T] {
  var content: List[T] = List.empty

  def pop(): T = {
    val next = peek()
    content = content.init
    return next
  }
  def push(element: T) = {
    content = element :: content
  }
  def peek() = content.last
  def length = content.length
  def size = length
  def clear() = {
    content = List.empty
  }
}

case class Rectangle(coord: Coord, width: Int, height: Int) {
  def w = width
  def h = height

  def minX = coord.x
  def maxX = coord.x + width - 1

  def minY = coord.y
  def maxY = coord.y + height - 1

  def tl = (minX, minY)
  def tr = (maxX, minY)
  def bl = (minX, maxY)
  def br = (maxX, maxY)

  def isIn(point: Coord): Boolean =
    (
      (minX <= point(0) && point(0) <= maxX) &&
        (minY <= point(1) && point(1) <= maxY)
    )

  def translate(by: Coord): Rectangle =
    Rectangle(coord + by, width, height)

  def scale(by: Int): Rectangle =
    Rectangle(coord, width * by, height * by)

  def scale(by: Coord): Rectangle =
    Rectangle(coord, width * by.x, height * by.y)

  def union(w: Rectangle): Rectangle =
    Rectangle(
      (Math.min(w.minX, minX), Math.min(w.minY, minY)),
      Math.max(w.maxX, maxX) - Math.min(w.minX, minX),
      Math.max(w.maxY, maxY) - Math.min(w.minY, minY)
    )
}

object Logger {

  val logs = new ListBuffer[String]()

  def log(msg: String, end: String = "\n") = {
    logs.addOne(msg + end)
  }

  def dump() = {
    logs.foreach(System.out.print)
    System.out.flush()
  }
}
