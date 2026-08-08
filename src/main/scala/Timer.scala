sealed trait Timer(duration: Int) {
  def isFinished: Boolean = this.isInstanceOf[Finished]
  def reset(count: Int = duration): Timer = Running(count, duration)
}

case class Running(count: Int, duration: Int) extends Timer(duration) {
  def this(duration: Int) = this(duration, duration)
  def tick(): Timer =
    if count > 1 then Running(count - 1, duration) else Finished(duration)
  def pause(): Timer = Paused(count, duration)
}

case class Paused(count: Int, duration: Int) extends Timer(duration) {
  def this(duration: Int) = this(duration, duration)
  def tick(): Timer = this
  def resume(): Timer = reset(count)
}

case class Finished(duration: Int) extends Timer(duration)
