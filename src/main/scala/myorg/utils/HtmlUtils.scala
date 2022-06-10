package myorg.utils

import tyrian.Html
import tyrian.Html.div

object HtmlUtils {

  def maybeElem[A, Msg](option: Option[A])(
      f: A => Html[Msg]
  ): Html[Msg] =
    option.fold(div())(f)
}
