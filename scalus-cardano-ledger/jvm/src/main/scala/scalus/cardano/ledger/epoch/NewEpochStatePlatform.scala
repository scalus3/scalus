package scalus.cardano.ledger.epoch

import scalus.InteropApi
import scalus.cardano.ledger.PoolKeyHash

import scala.jdk.CollectionConverters.*

/** The Java getters of [[NewEpochState]], interop style guide Tier 1: Java collections and `null`
  * instead of `Option`. The Scala.js copy of this trait is empty.
  */
private[epoch] trait NewEpochStatePlatform extends InteropApi { self: NewEpochState =>

    /** [[NewEpochState.blocksMadePrev]] as a Java map. */
    final def getBlocksMadePrev: java.util.Map[PoolKeyHash, java.lang.Long] =
        blocksMadePrev.view.mapValues(Long.box).toMap.asJava

    /** [[NewEpochState.blocksMadeCur]] as a Java map. */
    final def getBlocksMadeCur: java.util.Map[PoolKeyHash, java.lang.Long] =
        blocksMadeCur.view.mapValues(Long.box).toMap.asJava

    /** [[NewEpochState.pendingRewards]], or `null` before the first boundary. */
    final def getPendingRewardsOrNull: RewardUpdate = pendingRewards.orNull
}

/** The Java getters of [[PoolDistr]], interop style guide Tier 1. The Scala.js copy is empty. */
private[epoch] trait PoolDistrPlatform extends InteropApi { self: PoolDistr =>

    /** [[PoolDistr.pools]] as a Java map. */
    final def getPools: java.util.Map[PoolKeyHash, IndividualPoolStake] = pools.asJava
}

/** The Java getters of [[NonMyopic]], interop style guide Tier 1. The Scala.js copy is empty. */
private[epoch] trait NonMyopicPlatform extends InteropApi { self: NonMyopic =>

    /** [[NonMyopic.likelihoods]] as a Java map of Java lists. */
    final def getLikelihoods: java.util.Map[PoolKeyHash, java.util.List[java.lang.Float]] =
        likelihoods.view.mapValues(_.map(Float.box).asJava).toMap.asJava
}
