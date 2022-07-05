package volk.util

object CaseConverter {

  def camelCaseToSnakeCase: String => String = cc => {
    def go(rest: List[Char]): List[Char] = {
      rest match {
        case Nil                  => Nil
        case x :: xs if x.isUpper => '_' :: x.toLower :: go(xs)
        case x :: xs              => x :: go(xs)
      }
    }

    cc.toList match {
      case Nil     => ""
      case x :: xs => x + go(xs).mkString("")
    }
  }

}
