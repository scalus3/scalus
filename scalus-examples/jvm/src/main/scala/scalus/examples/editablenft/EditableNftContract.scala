package scalus.examples.editablenft

import scalus.compiler.Options
import scalus.uplc.PlutusV3

private given Options = Options.release

lazy val EditableNftContract = PlutusV3.compile(EditableNftValidator.validate)
