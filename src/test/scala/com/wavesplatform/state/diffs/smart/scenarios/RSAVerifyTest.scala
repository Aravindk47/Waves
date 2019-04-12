package com.wavesplatform.state.diffs.smart.scenarios

import com.wavesplatform.account.PrivateKeyAccount
import com.wavesplatform.block.Block
import com.wavesplatform.common.crypto.RSA._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils._
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.StdLibVersion.V1
import com.wavesplatform.lang.v1.compiler.ExpressionCompiler
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.state.diffs._
import com.wavesplatform.state.{BinaryDataEntry, Diff}
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.smart.script.Script
import com.wavesplatform.transaction.smart.script.v1.ExprScript
import com.wavesplatform.transaction.{DataTransaction, GenesisTransaction, ValidationError}
import com.wavesplatform.utils.compilerContext
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class RSAVerifyTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {

  property("return true on correct signature") {

    forAll(byteArrayGen(64)) { message =>
      val keyPair = createKeyPair()

      val pk  = keyPair.publicKey
      val sig = sign(message, keyPair.privateKey)

      rsaTest(message, pk, sig) { diffEi =>
        diffEi shouldBe an[Right[_, _]]
      }
    }
  }

  property("return false on incorrect signature") {
    forAll(byteArrayGen(64), byteArrayGen(32)) { (message1, message2) =>
      val kp = createKeyPair()

      val pk  = kp.publicKey
      val sig = sign(message2, kp.privateKey)

      rsaTest(message1, pk, sig) { diffEi =>
        diffEi shouldBe an[Left[_, _]]
      }
    }
  }

  property("return false on incorrect public key") {
    forAll(byteArrayGen(64)) { message =>
      val signerKeyPair = createKeyPair()

      val otherKeyPair = createKeyPair()

      val sig = sign(message, signerKeyPair.privateKey)

      rsaTest(message, otherKeyPair.publicKey, sig) { diffEi =>
        diffEi shouldBe an[Left[_, _]]
      }
    }
  }

  val WAVES: Long = 10000000
  val AMT: Long   = 1000000 * WAVES
  val FEE: Long   = 10 * WAVES

  val preconditions: Gen[(Block, PrivateKeyAccount)] = {
    accountGen.map { pka =>
      val genesisTx = GenesisTransaction
        .create(pka, AMT, System.currentTimeMillis())
        .explicitGet()

      val block = TestBlock
        .create(
          time = System.currentTimeMillis(),
          txs = Seq(genesisTx)
        )

      (block, pka)
    }
  }

  def rsaTest(msg: Array[Byte], xpub: Array[Byte], sig: Array[Byte])(f: Either[ValidationError, Diff] => Unit): Unit = {
    val scriptSrc =
      s"""
         |match tx {
         |  case dt: DataTransaction =>
         |    let msg  = extract(getBinary(dt.data, "signed_message"))
         |    let xpub = extract(getBinary(dt.data, "public_key"))
         |    let sig  = extract(getBinary(dt.data, "signature"))
         |    
         |    rsaSigVerify(msg, xpub, sig)
         |  case _ => false
         |}
     """.stripMargin

    val compiled = compile(scriptSrc)

    val settings = TestFunctionalitySettings.Stub.copy(
      preActivatedFeatures = BlockchainFeatures.implemented
        .map(_ -> 0)
        .toMap
    )

    forAll(preconditions) {
      case (block, sender) =>
        val setScriptTx = SetScriptTransaction
          .selfSigned(
            sender,
            Some(compiled),
            FEE,
            System.currentTimeMillis()
          )
          .explicitGet()
        val dataTx = DataTransaction
          .selfSigned(
            sender,
            List(
              BinaryDataEntry("signed_message", ByteStr(msg)),
              BinaryDataEntry("public_key", ByteStr(xpub)),
              BinaryDataEntry("signature", ByteStr(sig))
            ),
            FEE,
            System.currentTimeMillis()
          )
          .explicitGet()

        val blockWithScript = TestBlock
          .create(txs = Seq(setScriptTx))

        val blockWithData = TestBlock
          .create(txs = Seq(dataTx))

        assertDiffEi(
          Seq(block, blockWithScript),
          blockWithData,
          settings
        )(f)
    }
  }

  def compile(str: String): Script = {
    val untyped = Parser
      .parseExpr(str)
      .get
      .value

    val (typed, _) = ExpressionCompiler(
      compilerContext(V1, isAssetScript = false),
      untyped
    ).explicitGet()

    ExprScript(V1, typed).explicitGet()
  }
}
