package pimpathon


object option {
  implicit class OptionOps[A](val option: Option[A]) extends AnyVal {
    def getOrThrow(message: String): A = getOrThrow(new NoSuchElementException(message))
    def getOrThrow(exception: Exception): A = option.getOrElse(throw exception)
  }
}
