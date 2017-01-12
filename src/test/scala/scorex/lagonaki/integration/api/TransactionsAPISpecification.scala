package scorex.lagonaki.integration.api

import org.scalatest.{BeforeAndAfterAll, FunSuite, Matchers}
import play.api.libs.json.JsValue
import scorex.block.Block
import scorex.crypto.encode.Base58
import scorex.lagonaki.TransactionTestingCommons
import scorex.transaction.GenesisTransaction

class TransactionsAPISpecification extends FunSuite with Matchers with TransactionTestingCommons with BeforeAndAfterAll {

  import scorex.waves.TestingCommons._

  override def beforeAll(): Unit = {
    start()
    stopGeneration(applications)

    if (application.wallet.privateKeyAccounts().size < 10) application.wallet.generateNewAccounts(10)
  }

  override def afterAll(): Unit = {
    stop()
  }

  private def addresses = applicationNonEmptyAccounts.map(_.address)

  test("/transactions/unconfirmed API route") {
    (1 to 20) foreach (i => genValidTransaction())
    val unconfirmed = transactionModule.utxStorage.all()
    unconfirmed.size should be > 0
    val tr = GET.request("/transactions/unconfirmed")
    (tr \\ "signature").toList.size shouldBe unconfirmed.size
  }

  test("/transactions/address/{address}/limit/{limit} API route") {
    addresses.foreach { a =>
      val tr = GET.request(s"/transactions/address/$a/limit/2")
      (tr \\ "amount").toList.size should be <= 2
      checkTransactionList(tr)
    }
  }

  test("/transactions/address/{address}/limit/{limit} with invalid limit value") {
    val response = GET.requestRaw("/transactions/address/1/limit/f")
    assert(response.getStatusCode == 404)
  }

  test("/transactions/info/{signature} API route") {
    val genesisTx = Block.genesis().transactions.head.asInstanceOf[GenesisTransaction]
    val tr = GET.request(s"/transactions/info/${Base58.encode(genesisTx.signature)}")
    (tr \ "signature").as[String] shouldBe Base58.encode(genesisTx.signature)
    (tr \ "type").as[Int] shouldBe 1
    (tr \ "fee").as[Int] shouldBe 0
    (tr \ "amount").as[Long] should be > 0L
    (tr \ "height").as[Int] shouldBe 1
    (tr \ "recipient").as[String] shouldBe genesisTx.recipient.address
  }

  def checkTransactionList(tr: JsValue): Unit = {
    (tr \\ "amount").toList.foreach(amount => amount.as[Long] should be > 0L)
    (tr \\ "fee").toList.foreach(amount => amount.as[Long] should be >= 0L)
    (tr \\ "type").toList.foreach(amount => amount.as[Int] should be >= 0)
    (tr \\ "timestamp").toList.foreach(amount => amount.as[Long] should be >= 0L)
    (tr \\ "signature").toList.size should be >= 0
    (tr \\ "sender").toList.size should be >= 0
    (tr \\ "recipient").toList.size should be >= 0
  }


}
