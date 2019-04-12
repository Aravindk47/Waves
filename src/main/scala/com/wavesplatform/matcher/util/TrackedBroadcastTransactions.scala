package com.wavesplatform.matcher.util

import java.util.concurrent.ConcurrentSkipListMap

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.matcher.MatcherSettings.BroadcastUntilConfirmedSettings
import com.wavesplatform.transaction.assets.exchange.ExchangeTransaction

import scala.collection.immutable.SortedSet
import scala.concurrent.duration.{Deadline, Duration}

class TrackedBroadcastTransactions {
  private type Key = (Long, ByteStr) // Two transactions can have same timestamp
  private val storage = new ConcurrentSkipListMap[Key, ExchangeTransaction](Ordering[Key])

  def add(sendMs: Long, tx: ExchangeTransaction): Unit = storage.put(key(sendMs, tx), tx)
  def refresh(prevSendMs: Long, nextSendMs: Long, tx: ExchangeTransaction): Unit = {
    storage.remove(key(prevSendMs, tx))
    add(nextSendMs, tx)
  }

  def remove(lastSendMs: Long, tx: ExchangeTransaction): Unit = storage.remove(key(lastSendMs, tx))

  def ready(fromMs: Long): Iterator[(Long, ExchangeTransaction)] = {
    import scala.collection.JavaConverters._
    val startKey = (fromMs, ByteStr.empty)
    storage
      .tailMap(startKey)
      .asScala
      .iterator // here, because lastSendMs could be same for multiple transactions
      .map {
        case ((lastSendMs, _), tx) => (lastSendMs, tx)
      }
  }

  private def key(ms: Long, tx: ExchangeTransaction): Key = (ms, tx.id())
}

object BroadcastUntilConfirmed {
  case class TrackedTransaction(send: Deadline, expire: Deadline, tx: ExchangeTransaction)

  def process(settings: BroadcastUntilConfirmedSettings, now: Duration, txs: SortedSet[TrackedTransaction]): Unit = {
    val (expired, watched) = txs.iterator.span(_._2.timestamp <= expireMs)

    val failedToSend = expired.collect { case (_, tx) if !blockchain.containsTransaction(tx) => tx.id() }
    expired.foreach(Function.tupled(trackedBroadcastTxs.remove))

    val toSend = watched.filter { case (_, tx) => !blockchain.containsTransaction(tx) }
    toSend.foreach {
      case (lastSendMs, tx) =>
        allChannels.broadcastTx(tx)
        trackedBroadcastTxs.refresh(lastSendMs, nowMs, tx)
    }

    if (failedToSend.nonEmpty) log.error(s"Failed to send to the network: ${failedToSend.mkString(", ")}")
    log.info(
      s"Pending transactions stats: ${toSend.size} to send, ${watched.size} total watched, ${failedToSend.size} failed to send, ${expired.size} total expired"
    )
  }
}