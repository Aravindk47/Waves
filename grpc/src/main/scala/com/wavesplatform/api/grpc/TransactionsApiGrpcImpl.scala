package com.wavesplatform.api.grpc
import com.google.protobuf.empty.Empty
import com.wavesplatform.account.PublicKey
import com.wavesplatform.api.common.CommonTransactionsApi
import com.wavesplatform.api.http.TransactionNotExists
import com.wavesplatform.protobuf.transaction.{PBSignedTransaction, PBTransaction}
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.ValidationError
import com.wavesplatform.transaction.ValidationError.GenericError
import com.wavesplatform.utx.UtxPool
import com.wavesplatform.wallet.Wallet
import io.grpc.stub.StreamObserver
import io.netty.channel.group.ChannelGroup
import monix.execution.Scheduler
import monix.reactive.Observable

import scala.concurrent.Future
import scala.util.Try

class TransactionsApiGrpcImpl(functionalitySettings: FunctionalitySettings,
                              wallet: Wallet,
                              blockchain: Blockchain,
                              utx: UtxPool,
                              allChannels: ChannelGroup)(implicit sc: Scheduler)
    extends TransactionsApiGrpc.TransactionsApi {

  private[this] val commonApi = new CommonTransactionsApi(functionalitySettings, wallet, blockchain, utx, allChannels)

  override def getTransactionsByAddress(request: TransactionsByAddressRequest, responseObserver: StreamObserver[PBSignedTransaction]): Unit = {
    val stream = commonApi
      .transactionsByAddress(request.getAddress.toAddress, Option(request.fromId.toByteStr).filterNot(_.isEmpty))
      .map(_._2.toPB)

    responseObserver.completeWith(stream)
  }

  override def getTransactionById(request: TransactionByIdRequest): Future[PBSignedTransaction] = {
    commonApi
      .transactionById(request.transactionId)
      .map(_._2.toPB)
      .toFuture(TransactionNotExists)
  }

  override def getUnconfirmedTransactions(request: Empty, responseObserver: StreamObserver[PBSignedTransaction]): Unit = {
    val stream = Observable(commonApi.unconfirmedTransactions().map(_.toPB): _*)
    responseObserver.completeWith(stream)
  }

  override def getUnconfirmedTransactionById(request: TransactionByIdRequest): Future[PBSignedTransaction] = {
    commonApi
      .unconfirmedTransactionById(request.transactionId)
      .map(_.toPB)
      .toFuture(TransactionNotExists)
  }

  override def calculateFee(request: PBTransaction): Future[CalculateFeeResponse] = {
    commonApi.calculateFee(request.toVanilla).map { case (assetId, assetAmount, _) => CalculateFeeResponse(assetId.protoId, assetAmount) }.toFuture
  }

  override def signTransaction(request: TransactionSignRequest): Future[PBSignedTransaction] = {
    def signTransactionWith(tx: PBTransaction, wallet: Wallet, signerAddress: String): Either[ValidationError, PBSignedTransaction] =
      for {
        sender <- wallet.findPrivateKey(tx.sender.toString)
        signer <- if (tx.sender.toString == signerAddress) Right(sender) else wallet.findPrivateKey(signerAddress)
        tx     <- Try(tx.signed(signer.privateKey)).toEither.left.map(GenericError(_))
      } yield tx

    val signerAddress: PublicKey = if (request.signer.isEmpty) request.getTransaction.sender else request.signer.toPublicKeyAccount
    signTransactionWith(request.getTransaction, wallet, signerAddress.toString).toFuture
  }

  override def broadcastTransaction(tx: PBSignedTransaction): Future[PBSignedTransaction] = {
    commonApi
      .broadcastTransaction(tx.toVanilla)
      .map(_.toPB)
      .toFuture
  }
}
