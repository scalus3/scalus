package scalus.cardano.network.chainsync

import scalus.cardano.node.stream.StartFrom

import scala.concurrent.{ExecutionContext, Future}

/** Translates the streaming-core [[StartFrom]] subscription policy into the wire-level
  * [[ChainSyncMessage.MsgFindIntersect]] sequence and drives it against a [[ChainSyncDriver]] to
  * return the point the peer actually anchored on.
  *
  * ChainSync has no "start from your tip" wire message; for `StartFrom.Tip` we run a two-step
  * dance: first `FindIntersect([Origin])` to learn the peer's tip from its reply, then
  * `FindIntersect([tip])` to commit. Between the two steps a new block may land on the peer — the
  * peer's header store still has `tip` so the second intersect succeeds, and the first `next()`
  * returns the block produced in the window. No events are missed.
  *
  * `StartFrom.Origin` and `StartFrom.At(p)` are single-step: one `FindIntersect` with the sole
  * candidate. Origin is usable on dev chains (yaci devnet starts in Conway/Babbage so no Byron
  * decoder is needed); `At(p)` is the checkpoint-resume case (M7). In M5 the provider always passes
  * `StartFrom.Tip`; the other cases exist so M7's replay path wires onto the same code.
  */
object IntersectSeeker {

    /** Drive the appropriate FindIntersect sequence against the peer.
      *
      * On success returns the peer-confirmed [[Point]] (not the tip — the caller's chain applier is
      * about to start pulling blocks; tip observation lives in subsequent `next()` results). On
      * failure the returned future carries [[scalus.cardano.network.ChainSyncError]] (most commonly
      * `NoIntersection` for an `At(p)` that the peer doesn't know about).
      */
    def seek(
        driver: ChainSyncDriver,
        startFrom: StartFrom
    )(using ExecutionContext): Future[Point] = startFrom match {
        case StartFrom.Origin =>
            driver.findIntersect(Seq(Point.Origin)).map(_._1)

        case StartFrom.At(p) =>
            driver.findIntersect(Seq(Point.fromChainPoint(p))).map(_._1)

        case StartFrom.Tip =>
            // Two-step: learn peer tip by intersecting at Origin (always succeeds, returns peer
            // tip in the reply), then re-intersect at that tip to commit.
            driver
                .findIntersect(Seq(Point.Origin))
                .flatMap { case (_, peerTip) =>
                    // peerTip.point may itself be Origin on a freshly-started chain; in that
                    // case the second intersect is a no-op that returns us to Idle at Origin —
                    // and the very next `next()` returns the genesis block when it appears.
                    driver.findIntersect(Seq(peerTip.point)).map(_._1)
                }
    }
}
