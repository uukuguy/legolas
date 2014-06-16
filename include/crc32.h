#ifndef __CRC32_H__
#define __CRC32_H__

#include <stdint.h>

typedef uint32_t z_crc_t;

z_crc_t crc32(z_crc_t crc, const unsigned char  *buf, uint32_t len);
z_crc_t  crc32_combine(z_crc_t crc1, z_crc_t crc2, uint32_t len2);

#endif /* __CRC32_H__ */


