package com.wavesplatform.matcher.market

import akka.actor.Actor
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.matcher.MatcherSettings.BroadcastUntilConfirmedSettings
import com.wavesplatform.matcher.market.BroadcastUntilConfirmedActor._
import com.wavesplatform.matcher.model.Events.ExchangeTransactionCreated
import com.wavesplatform.transaction.assets.exchange.ExchangeTransaction
import com.wavesplatform.utils.{ScorexLogging, Time}

import scala.collection.immutable.SortedSet

class BroadcastUntilConfirmedActor(settings: BroadcastUntilConfirmedSettings,
                                   time: Time,
                                   isConfirmed: ExchangeTransaction => Boolean,
                                   broadcast: Seq[ExchangeTransaction] => Unit)
    extends Actor with ScorexLogging {

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[ExchangeTransactionCreated])
  }

  override def receive: Receive = state(
    allTxs = SortedSet.empty[ExchangeTransaction](Ordering.by[ExchangeTransaction, Key](tx => (tx.timestamp, tx.id()))),
    lastSendTxs = Set.empty[ByteStr]
  )

  private def state(allTxs: SortedSet[ExchangeTransaction], lastSendTxs: Set[ByteStr]): Receive = {
    case ExchangeTransactionCreated(tx) =>
      broadcast(List(tx))
      context.become(state(allTxs + tx, lastSendTxs + tx.id()))

    case Send =>
      val nowMs    = time.getTimestamp()
      val readyMs  = nowMs - settings.interval.toMillis
      val expireMs = nowMs - settings.maxPendingTime.toMillis

      val toCheck                  = allTxs.iterator.takeWhile(_.timestamp <= readyMs).filterNot(tx => lastSendTxs.contains(tx.id()))
      val (confirmed, unconfirmed) = toCheck.toVector.partition(isConfirmed)
      val (expired, ready)         = unconfirmed.partition(_.timestamp <= expireMs)

      broadcast(ready)
      val sent: Set[ByteStr] = ready.map(_.id())(collection.breakOut)

      log.debug(s"Pending transactions stats: ${sent.size} to send, ${expired.size} failed to send")
      context.become(state(allTxs -- confirmed -- expired, sent))
  }
}

object BroadcastUntilConfirmedActor {
  private object Send
  private type Key = (Long, ByteStr) // Two transactions can have same timestamp
}
