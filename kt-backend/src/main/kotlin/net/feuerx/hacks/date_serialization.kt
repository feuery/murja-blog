package net.feuerx.hacks

import kotlinx.serialization.ExperimentalSerializationApi
import kotlinx.serialization.Serializer
import kotlinx.serialization.encoding.Decoder
import kotlinx.serialization.encoding.Encoder
import java.time.format.DateTimeFormatter
import java.time.LocalDateTime
import java.util.Locale

@OptIn(ExperimentalSerializationApi::class)
@Serializer(forClass = LocalDateTime::class)
object DateTimeSerializer {
    // 2019-11-22T18:26:45Z
    private val format = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:SS'Z'")

    override fun deserialize(decoder: Decoder): LocalDateTime {
        val dateString = decoder.decodeString()
        return LocalDateTime.parse(dateString, format)
    }

    override fun serialize(encoder: Encoder, value: LocalDateTime) {
        val dateString = value.format(format)
        encoder.encodeString(dateString)
    }
}
