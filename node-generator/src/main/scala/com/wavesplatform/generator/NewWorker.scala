package com.wavesplatform.generator

import java.net.{InetSocketAddress, URL}

import com.wavesplatform.generator.Worker.Settings
import com.wavesplatform.network.RawBytes
import com.wavesplatform.network.client.NetworkSender
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.utils.ScorexLogging
import io.netty.channel.Channel
import monix.eval.Task
import monix.execution.Scheduler
import org.asynchttpclient.AsyncHttpClient
import play.api.libs.json.Json

import scala.compat.java8.FutureConverters
import scala.concurrent.{ExecutionContext, Future}

class NewWorker(settings: Settings,
                transactionSource: Iterator[Transaction],
                networkSender: NetworkSender,
                node: InetSocketAddress,
                nodeRestAddress: URL,
                canContinue: () => Boolean,
                initial: Seq[RawBytes])(implicit httpClient: AsyncHttpClient, ec: ExecutionContext)
    extends ScorexLogging {

  def run(): Future[Unit] =
    pullAndWriteTask().map(_ => ()).runAsyncLogErr(Scheduler(ec))

  private[this] def utxSpace: Task[Int] = Task.defer {
    import org.asynchttpclient.Dsl._
    val request = get(s"$nodeRestAddress/transactions/unconfirmed/size").build()
    Task
      .fromFuture(FutureConverters.toScala(httpClient.executeRequest(request).toCompletableFuture))
      .map(r => (Json.parse(r.getResponseBody) \ "size").as[Int])
  }

  private[this] def writeTransactions(channel: Channel, txs: Seq[Transaction]): Task[Unit] = Task.fromFuture {
    for {
      _ <- networkSender.send(channel, initial: _*)
      _ = log.info(s"Sending ${txs.length} to $channel")
      _ <- networkSender.send(channel, txs.map(RawBytes.from): _*)
    } yield ()
  }

  private[this] def pullAndWriteTask(channel: Channel = null): Task[Channel] =
    if (!canContinue()) Task.now(null)
    else {
      val baseTask = for {
        validChannel <- if (channel != null && channel.isOpen) Task.now(channel)
        else Task.fromFuture(networkSender.connect(node))

        txCount <- utxSpace
        _       <- writeTransactions(validChannel, transactionSource.take(settings.utxLimit - txCount).toVector)
      } yield validChannel

      val withReconnect: Task[Channel] = baseTask.onErrorRecoverWith {
        case e =>
          channel.close()

          if (settings.autoReconnect) {
            log.error(s"[$node] An error during sending transations, reconnect", e)
            for {
              _       <- Task.sleep(settings.reconnectDelay)
              channel <- pullAndWriteTask()
            } yield channel
          } else {
            log.error("Stopping because autoReconnect is disabled", e)
            Task.raiseError(e)
          }
      }

      withReconnect.flatMap(channel => Task.sleep(settings.delay).flatMap(_ => pullAndWriteTask(channel)))
    }
}
