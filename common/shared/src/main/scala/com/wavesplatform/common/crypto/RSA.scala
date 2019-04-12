package com.wavesplatform.common.crypto

import java.security.spec.{PKCS8EncodedKeySpec, X509EncodedKeySpec}
import java.security.{KeyFactory, KeyPairGenerator, SecureRandom, Signature => JSignature}

import supertagged._

object RSA {

  val KeyLength: Int = 2048

  object Signature extends TaggedType[Array[Byte]]
  type Signature = Signature.Type

  object PrivateKey extends TaggedType[Array[Byte]]
  type PrivateKey = PrivateKey.Type

  object PublicKey extends TaggedType[Array[Byte]]
  type PublicKey = PublicKey.Type

  final case class KeyPair(publicKey: PublicKey, privateKey: PrivateKey)

  def createKeyPair(): KeyPair = {
    val generator = KeyPairGenerator.getInstance("RSA")
    generator.initialize(KeyLength, new SecureRandom)
    val kp = generator.generateKeyPair

    val xprv = PrivateKey @@ kp.getPrivate.getEncoded
    val xpub = PublicKey @@ kp.getPublic.getEncoded

    KeyPair(xpub, xprv)
  }

  def sign(message: Array[Byte], privateKey: PrivateKey): Signature = {
    val privateSignature = JSignature.getInstance("SHA256withRSA")
    val keyFactory       = KeyFactory.getInstance("RSA")

    val key = keyFactory.generatePrivate(new PKCS8EncodedKeySpec(privateKey))

    privateSignature.initSign(key)
    privateSignature.update(message)

    Signature @@ privateSignature.sign
  }

  def verify(message: Array[Byte], publicKey: PublicKey, signature: Signature): Boolean = {
    val publicSignature = JSignature.getInstance("SHA256withRSA")

    val keyFactory = KeyFactory.getInstance("RSA")

    val key = keyFactory.generatePublic(new X509EncodedKeySpec(publicKey))

    publicSignature.initVerify(key)
    publicSignature.update(message)
    publicSignature.verify(signature)
  }

}
