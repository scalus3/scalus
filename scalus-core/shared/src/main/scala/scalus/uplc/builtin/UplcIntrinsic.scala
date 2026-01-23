package scalus.uplc.builtin

import scala.annotation.StaticAnnotation
import scala.annotation.nowarn

@nowarn("msg=unused explicit parameter")
final class uplcIntrinsic(value: String) extends StaticAnnotation
