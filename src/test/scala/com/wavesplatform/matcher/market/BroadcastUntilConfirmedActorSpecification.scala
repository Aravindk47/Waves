package com.wavesplatform.matcher.market

import akka.testkit.{ImplicitSender, TestActorRef}
import com.wavesplatform.account.PrivateKeyAccount
import com.wavesplatform.matcher.MatcherSettings.BroadcastUntilConfirmedSettings
import com.wavesplatform.matcher.MatcherTestData
import com.wavesplatform.matcher.model.Events.ExchangeTransactionCreated
import com.wavesplatform.transaction.Proofs
import com.wavesplatform.transaction.assets.exchange.{AssetPair, ExchangeTransaction, ExchangeTransactionV2, Order}
import com.wavesplatform.utils.Time
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.BeforeAndAfterEach
import org.scalatest.concurrent.Eventually

import scala.concurrent.duration.DurationInt

class BroadcastUntilConfirmedActorSpecification
    extends MatcherSpec("BroadcastUntilConfirmedActor")
    with MatcherTestData
    with BeforeAndAfterEach
    with PathMockFactory
    with ImplicitSender
    with Eventually {

  "BroadcastUntilConfirmedActor" should {
    "broadcast a transaction when receives it" in {
      system.eventStream.publish(ExchangeTransactionCreated())
    }

    "broadcast a transaction if it wasn't confirmed" in {}
    "broadcast a transaction in next period" in {}
    "doesn't broadcast a transaction if it was confirmed" in {}
  }

  private def defaultActor(time: Time, isConfirmed: ExchangeTransaction => Boolean, broadcast: Seq[ExchangeTransaction] => Unit): TestActorRef[BroadcastUntilConfirmedActor] = TestActorRef(
    new BroadcastUntilConfirmedActor(
      settings = BroadcastUntilConfirmedSettings(
        enable = true,
        interval = 1.minute,
        maxPendingTime = 5.minute
      ),
      time = time,
      isConfirmed = isConfirmed,
      broadcast = broadcast
    )
  )

  private def sampleExchangeTx(): ExchangeTransaction = ExchangeTransactionV2.create(
    buyOrder = Order.buy(
      sender = PrivateKeyAccount(Array.emptyByteArray),
      matcher = PrivateKeyAccount(Array.emptyByteArray),
      pair = AssetPair(None, None),
      amount = 100,
      price = 6000000L,
      timestamp = 100,
      expiration = 101,
      matcherFee = 100
    ),
    sellOrder = Order.sell(
      sender = PrivateKeyAccount(Array.emptyByteArray),
      matcher = PrivateKeyAccount(Array.emptyByteArray),
      pair = AssetPair(None, None),
      amount = 100,
      price = 6000000L,
      timestamp = 100,
      expiration = 101,
      matcherFee = 100
    ),
    amount = 100,
    price = 6000000L,
    buyMatcherFee = 0L,
    sellMatcherFee = 0L,
    fee = 0L,
    timestamp = System.currentTimeMillis(),
    proofs = Proofs.empty
  ).right.get
}
