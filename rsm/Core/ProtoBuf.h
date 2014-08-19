/* Copyright (c) 2012 Stanford University
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR(S) DISCLAIM ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL AUTHORS BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

#include <google/protobuf/message.h>
#include <google/protobuf/text_format.h>
#include <memory>
#include <string>

/**
 * \file
 * Utilities for dealing with protocol buffers.
 */

#ifndef LOGCABIN_CORE_PROTOBUF_H
#define LOGCABIN_CORE_PROTOBUF_H

namespace google {
namespace protobuf {

/**
 * Equality for protocol buffers so that they can be used in EXPECT_EQ.
 * This is useful for testing.
 */
bool operator==(const Message& a, const Message& b);

/**
 * Inequality for protocol buffers so that they can be used in EXPECT_NE.
 * This is useful for testing.
 */
bool operator!=(const Message& a, const Message& b);

// Equality and inequality between protocol buffers and their text format
// representations. These are useful for testing.
bool operator==(const Message& a, const std::string& b);
bool operator==(const std::string& a, const Message& b);
bool operator!=(const Message& a, const std::string& b);
bool operator!=(const std::string& a, const Message& b);

} // namespace google::protobuf
} // namespace google

namespace LogCabin {
namespace Core {
namespace ProtoBuf {

namespace Internal {

/// Helper for fromString template.
void fromString(const std::string& str, google::protobuf::Message& protoBuf);

} // namespace LogCabin::ProtoBuf::Internal

/**
 * Create a protocol buffer message form a text format.
 * This is useful for testing.
 * \tparam ProtoBuf
 *      A derived class of ProtoBuf::Message.
 * \param str
 *      The string representation of the protocol buffer message.
 *      Fields that are missing will not throw an error, but the resulting
 *      protocol buffer may be less useful.
 * \return
 *      The parsed protocol buffer.
 */
template<typename ProtoBuf>
ProtoBuf
fromString(const std::string& str)
{
    ProtoBuf protoBuf;
    Internal::fromString(str, protoBuf);
    return protoBuf;
}

/**
 * Dumps a protocol buffer message.
 * This is useful for debugging and for testing.
 *
 * \param protoBuf
 *      The protocol buffer message to dump out. It is safe to call this even
 *      if you haven't filled in all required fields, but the generated string
 *      will not be directly parse-able.
 * \param forCopyingIntoTest
 *      If set to true, this will return a string in a format most useful for
 *      writing unit tests. You can basically copy and paste this from your
 *      terminal into your test file without manual processing. If set to false
 *      (default), the output will be nicer to read but harder to copy into a
 *      test file.
 * \return
 *      Textual representation. This will be printable ASCII; binary will be
 *      escaped.
 */
std::string
dumpString(const google::protobuf::Message& protoBuf,
           bool forCopyingIntoTest = false);

/**
 * Copy the contents of a protocol buffer into a new one.
 */
std::unique_ptr<google::protobuf::Message>
copy(const google::protobuf::Message& protoBuf);

} // namespace LogCabin::Core::ProtoBuf
} // namespace LogCabin::Core
} // namespace LogCabin


#include "Buffer.h"

namespace LogCabin {
namespace RPC {
namespace ProtoBuf {

/**
 * Parse a protocol buffer message out of an Core::Buffer.
 * \param from
 *      The Core::Buffer from which to extract a protocol buffer.
 * \param[out] to
 *      The empty protocol buffer to fill in with the contents of the
 *      Core::Buffer.
 * \param skipBytes
 *      The number of bytes to skip at the beginning of 'from' (defaults to 0).
 * \return
 *      True if the protocol buffer was parsed successfully; false otherwise
 *      (for example, if a required field is missing).
 */
bool
parse(const Core::Buffer& from,
      google::protobuf::Message& to,
      uint32_t skipBytes = 0);

/**
 * Serialize a protocol buffer message into an Core::Buffer.
 * \param from
 *      The protocol buffer containing the contents to serialize into the
 *      Core::Buffer. All required fields must be set or this will PANIC.
 * \param[out] to
 *      The Core::Buffer to fill in with the contents of the protocol buffer.
 * \param skipBytes
 *      The number of bytes to allocate at the beginning of 'to' but leave
 *      uninitialized for someone else to fill in (defaults to 0).
 */
void
serialize(const google::protobuf::Message& from,
          Core::Buffer& to,
          uint32_t skipBytes = 0);

} // namespace LogCabin::RPC::ProtoBuf
} // namespace LogCabin::RPC
} // namespace LogCabin

#endif /* LOGCABIN_CORE_PROTOBUF_H */
