import org.jline.terminal.{Terminal, TerminalBuilder}
import scala.concurrent.duration._
import scala.util.control.Breaks._
import Coord.*

object Main {

  def keyToInput(c: Int): Option[GameBoard.Input] =
    c match
      case 'a'    => Some(GameBoard.Input.Left)
      case 'd'    => Some(GameBoard.Input.Right)
      case 's'    => Some(GameBoard.Input.Down)
      case 'w'    => Some(GameBoard.Input.Rotate)
      case _: Int => None

  def main(args: Array[String]): Unit = {
    val terminal: Terminal = TerminalBuilder
      .builder()
      .system(true)
      .jansi(true)
      .build()

    terminal.enterRawMode()
    val reader = terminal.reader()

    val boardDims: Coord = (10, 20)
    val boardRectangle = Rectangle(
      Coord.Origin,
      boardDims.x * Asset.Block.bounds.w + 2,
      boardDims.y * Asset.Block.bounds.h + 2
    )
    val pocketRectangle = Rectangle(
      boardRectangle.tr + (2, 0),
      4 * Asset.Block.bounds.w + 2,
      4 * Asset.Block.bounds.h + 2
    )

    val pocket = new Pocket(pocketRectangle.tl)
    val board = new GameBoard(boardDims.x, boardDims.y, pocket)

    val screen = new Screen(
      boardRectangle.union(pocketRectangle)
    )

    @volatile var running = true

    val inputThread = new Thread(() => {
      while (running) {
        val c = reader.read()
        c match
          case 'q'    => running = false
          case k: Int =>
            keyToInput(k).foreach(inp => board.inputQueue.push(inp))
      }
    })
    inputThread.setDaemon(true)
    inputThread.start()

    try {
      while (running) {
        board.process()

        screen.clear()

        screen.draw(board.rendered())

        screen.display()

        Thread.sleep(50)
      }
    } finally {
      terminal.close()
      Logger.dump()
    }
  }

}
