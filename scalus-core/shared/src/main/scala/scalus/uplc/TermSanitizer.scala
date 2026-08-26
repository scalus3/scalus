package scalus.uplc

import scala.collection.mutable

/** Sanitizes UPLC Terms to ensure all variable names conform to the identifier grammar accepted by
  * the reference `plutus-core` textual parser (`PlutusCore.Parser.ParserCommon.name`, backed by
  * `PlutusCore.Name.Unique.isIdentifierStartingChar` / `isIdentifierChar`; verified against
  * plutus-core-1.45.0.0):
  *
  *   - First character: an ASCII letter (a-z, A-Z) or underscore (_).
  *   - Subsequent characters: an ASCII letter, digit, underscore (_), or apostrophe (').
  *   - The reference parser additionally accepts a `-<digits>` "Unique" suffix (e.g. `foo-123`),
  *     but only as a *terminal* token component parsed by a separate combinator: once `-<digits>`
  *     starts, nothing else (not even more digits, letters, or apostrophes) may follow it in the
  *     same token. A name like `a-91533'653` therefore parses as `a-91533` followed by a stray,
  *     unparseable `'653`. Since Scalus never needs to emit that literal Unique-suffix syntax, this
  *     sanitizer does not attempt to reproduce it: every `-` in an original name is replaced with
  *     `_` instead, which is always valid in any position and sidesteps the terminal-suffix
  *     ambiguity entirely.
  *
  * This sanitizer transforms variable names to comply with these rules while avoiding conflicts.
  */
object TermSanitizer:
    import Term.*

    /** Sanitizes all variable names in a Term to be valid UPLC identifiers.
      *
      * @param term
      *   the term to sanitize
      * @return
      *   a new term with all variable names sanitized
      */
    def sanitizeNames(term: Term): Term =
        val usedNames = mutable.Set[String]()
        val renameMap = mutable.Map[String, String]()

        def getSanitizedName(original: String): String =
            renameMap.getOrElseUpdate(
              original, {
                  val sanitized = sanitizeName(original)
                  findUniqueName(sanitized, usedNames)
              }
            )

        def sanitizeTerm(t: Term): Term = t match
            case Var(name, ann) =>
                val sanitizedName = getSanitizedName(name.name)
                Var(name.copy(name = sanitizedName), ann)

            case LamAbs(name, body, ann) =>
                val sanitizedName = getSanitizedName(name)
                usedNames.add(sanitizedName)
                LamAbs(sanitizedName, sanitizeTerm(body), ann)

            case Apply(f, arg, ann) =>
                Apply(sanitizeTerm(f), sanitizeTerm(arg), ann)

            case Force(term, ann) =>
                Force(sanitizeTerm(term), ann)

            case Delay(term, ann) =>
                Delay(sanitizeTerm(term), ann)

            case _: Const => t

            case _: Builtin => t

            case _: Error => t

            case Constr(tag, args, ann) =>
                Constr(tag, args.map(sanitizeTerm), ann)

            case Case(scrutinee, cases, ann) =>
                Case(sanitizeTerm(scrutinee), cases.map(sanitizeTerm), ann)

        sanitizeTerm(term)

    /** Sanitizes a single name to conform to UPLC identifier rules.
      *
      * @param name
      *   the original name
      * @return
      *   a sanitized version of the name
      */
    def sanitizeName(name: String): String =
        if name.isEmpty then return "_empty"

        val chars = name.toCharArray
        val result = new StringBuilder

        // Handle first character - must be letter or underscore
        val firstChar = chars(0)
        if isValidFirstChar(firstChar) then result.append(firstChar)
        else if firstChar.isDigit then
            result.append('_')
            result.append(firstChar)
        else result.append('_')

        // Handle subsequent characters - can be letter, digit, underscore, or apostrophe.
        // Note: '-' is deliberately NOT special-cased here (see the class doc comment) - it is
        // always replaced with '_', even when followed by digits, because the reference parser
        // only accepts '-\d+' as a terminal suffix, not embeddable anywhere in the name.
        for i <- 1 until chars.length do
            val c = chars(i)
            if isValidSubsequentChar(c) then result.append(c)
            else result.append('_')

        result.toString

    /** Checks if a character is valid as the first character of a UPLC identifier.
      *
      * @param c
      *   the character to check
      * @return
      *   true if valid, false otherwise
      */
    private def isValidFirstChar(c: Char): Boolean =
        c.isLetter || c == '_'

    /** Checks if a character is valid as a subsequent character in a UPLC identifier.
      *
      * @param c
      *   the character to check
      * @return
      *   true if valid, false otherwise
      */
    private def isValidSubsequentChar(c: Char): Boolean =
        c.isLetterOrDigit || c == '_' || c == '\''

    /** Finds a unique name by appending a numeric suffix if the base name is already used.
      *
      * @param baseName
      *   the base name to start with
      * @param usedNames
      *   the set of already used names
      * @return
      *   a unique name not in usedNames
      */
    private def findUniqueName(baseName: String, usedNames: mutable.Set[String]): String =
        if !usedNames.contains(baseName) then
            usedNames.add(baseName)
            baseName
        else
            var counter = 1
            var candidate = s"$baseName$counter"
            while usedNames.contains(candidate) do
                counter += 1
                candidate = s"$baseName$counter"
            usedNames.add(candidate)
            candidate
