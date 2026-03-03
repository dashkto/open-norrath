package opennorrath.animation

import opennorrath.wld.{Fragment12_TrackDef, WldFile}

/** Indexed map of animation track definitions with derived animation code set.
  *
  * Track names follow the convention {CODE}{MODEL}{BONE} (old-style) or
  * {CODE}{A|B}{MODEL}{BONE} (Luclin-era). Animation codes are 3-char prefixes
  * like "C01" (combat), "L01" (walk), "P01" (idle).
  *
  * Centralizes the track lookup helpers that were previously inlined in
  * AnimatedCharacter, ZoneRenderer, GlobalCharacters, and MissingAnimSurvey.
  */
class TrackMap private (
    val byName: Map[String, Fragment12_TrackDef],
    val animCodes: Set[String],
):

  /** True if any track exists for this prefix alone or prefix+suffix combo. */
  def hasTracksForPrefix(prefix: String, nonPointSuffixes: Set[String]): Boolean =
    byName.contains(prefix) ||
      nonPointSuffixes.exists(s => byName.contains(prefix + s))

  /** Find the full animation prefix for a given 3-char code and model prefix.
    *
    * Old-style tracks: {code}{model} (e.g. "C01HUM")
    * Luclin-era tracks add a variant letter: {code}A{model} or {code}B{model}
    */
  def findPrefix(code: String, modelPrefix: String, nonPointSuffixes: Set[String]): Option[String] =
    val direct = code + modelPrefix
    if hasTracksForPrefix(direct, nonPointSuffixes) then Some(direct)
    else Iterator("A", "B").map(v => code + v + modelPrefix).find(p => hasTracksForPrefix(p, nonPointSuffixes))

object TrackMap:

  /** Build a TrackMap from a flat collection of track defs. Later entries win on name collision. */
  def from(trackDefs: Iterable[Fragment12_TrackDef]): TrackMap =
    val byName = trackDefs.map(td => td.cleanName -> td).toMap
    val animCodes = byName.keysIterator.filter(_.length > 3).map(_.take(3)).toSet
    new TrackMap(byName, animCodes)

  /** Merge a base TrackMap with the track defs from a WLD file.
    * Local WLD tracks override base tracks of the same name.
    */
  def merge(base: TrackMap, wld: WldFile): TrackMap =
    merge(base, wld.fragmentsOfType[Fragment12_TrackDef])

  /** Merge a base TrackMap with an explicit extra list. */
  def merge(base: TrackMap, extra: Iterable[Fragment12_TrackDef]): TrackMap =
    val extraByName = extra.map(td => td.cleanName -> td).toMap
    val merged = base.byName ++ extraByName
    val animCodes = merged.keysIterator.filter(_.length > 3).map(_.take(3)).toSet
    new TrackMap(merged, animCodes)

  /** Empty TrackMap. */
  val empty: TrackMap = new TrackMap(Map.empty, Set.empty)
