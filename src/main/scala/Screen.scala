import Coord.*

// MARK: Scaffolding

type PixelValue = Char

trait Drawable {
  def coords: Seq[Coord]
  def render(pos: Coord): PixelValue
}

object Drawable {
  val Empty: Drawable = new Drawable {
    override def coords: Seq[Coord] = List.empty[Coord]
    override def render(pos: Coord): PixelValue = ' '
  }

  def compose(assets: Seq[Drawable]): Drawable =
    new Drawable { // TODO vvvv
      override def coords: Seq[Coord] = assets.flatMap(_.coords).distinct
      override def render(pos: Coord): PixelValue =
        assets.reverse
          .find(_.coords.contains(pos))
          .map(_.render(pos))
          .getOrElse(' ')
    }
}

// MARK: Palettes

trait Palette {
  def get(i: Int): PixelValue
  def size: Int
}

case class StringPalette(chars: String) extends Palette {
  def get(i: Int): Char = chars.charAt(i)
  def size: Int = chars.length()
}

object StringPalette {
  def join(char: Char*): StringPalette = new StringPalette(char.mkString)
}

object UIPalette extends StringPalette(" #$-|+") {
  def Blank: PixelValue = get(0)
  def Block: PixelValue = get(1)
  def Input: PixelValue = get(2)
  def HLine: PixelValue = get(3)
  def VLine: PixelValue = get(4)
  def Cross: PixelValue = get(5)

  def Outline: StringPalette = StringPalette.join(
    VLine,
    HLine,
    VLine,
    HLine,
    Cross
  )
}

// MARK: Shapes etc.

case class FillRectangle(bounds: Rectangle, value: PixelValue = ' ')
    extends Drawable {
  override def coords: Seq[Coord] =
    for
      x <- bounds.minX to bounds.maxX
      y <- bounds.minY to bounds.maxY
    yield (x, y)

  override def render(pos: Coord): PixelValue = value

}

case class OutlinedRectangle(
    bounds: Rectangle,
    palette: StringPalette = UIPalette.Outline
) extends Drawable {
  override def coords: Seq[Coord] = {
    val top = for x <- bounds.minX to bounds.maxX yield (x, bounds.minY)
    val bottom = for x <- bounds.minX to bounds.maxX yield (x, bounds.maxY)
    val left = for y <- bounds.minY to bounds.maxY yield (bounds.minX, y)
    val right = for y <- bounds.minY to bounds.maxY yield (bounds.maxX, y)

    return (top ++ bottom ++ left ++ right).distinct
  }

  override def render(pos: Coord): PixelValue = {
    val onLeft = pos.x == bounds.minX
    val onTop = pos.y == bounds.minY
    val onRight = pos.x == bounds.maxX
    val onBottom = pos.y == bounds.maxY
    val onLeftOrRight = onLeft || onRight
    val onTopOrBottom = onTop || onBottom

    if onLeftOrRight && onTopOrBottom then palette.get(4)
    else if onLeft then palette.get(0)
    else if onTop then palette.get(1)
    else if onRight then palette.get(2)
    else palette.get(3)
  }
}

case class Asset(bounds: Rectangle, palette: StringPalette, indices: Grid[Int])
    extends Drawable {
  override def coords: Seq[Coord] =
    for
      x <- bounds.minX to bounds.maxX
      y <- bounds.minY to bounds.maxY
    yield (x, y)

  override def render(pos: Coord): PixelValue =
    palette.get(indices(pos(1) - bounds.minY)(pos(0) - bounds.minX))

  def translate(by: Coord): Asset =
    copy(bounds = bounds.translate(by))
}

object Asset {
  def from(at: Coord, lines: String*): Asset = {
    val chars: String = lines.flatten.distinct.mkString
    val palette = StringPalette(chars)

    val indices: Grid[Int] =
      lines.map(line => line.map(c => chars.indexOf(c)).toArray).toArray

    if (lines.exists(line => line.length() != lines(0).length())) {
      throw new Error("Invalid asset definition")
    }

    val width = if lines.isEmpty then 0 else lines.map(_.length).max
    val height = if lines.isEmpty then 0 else lines.length

    Asset(Rectangle(at, width, height), palette, indices)
  }

  def from(lines: String*): Asset = {
    Asset.from(Origin, lines*)
  }

  val Block: Asset = Asset.from(
    "██"
  )
  val GhostBlock: Asset = Asset.from(
    "░░"
  )
}

// MARK: Screen

class Screen(bounds: Rectangle) {

  private val _screen: Grid[Char] = Array.fill(bounds.height, bounds.width)(' ')

  def draw(asset: Drawable): Unit = {
    for (coord <- asset.coords) {
      _screen(coord.y)(coord.x) = asset.render(coord)
    }
  }

  def clear(): Unit = {
    this.draw(new FillRectangle(bounds, ' '))
  }

  def display(): Unit = {
    val sb = new StringBuilder()
    sb.append("\u001b[2J")
    sb.append("\u001b[H")
    for (y <- 0 until bounds.height) {
      for (x <- 0 until bounds.width) {
        sb.append(this._screen(y)(x))
      }
      sb.append('\n')
    }
    print(sb.toString())
    System.out.flush()
  }

}
