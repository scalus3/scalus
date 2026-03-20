import blake2b from 'blake2b';


/* Size of the digest of the underlying hash algorithm.
 * @private
 */
export const DIGEST_LENGTH = 32; // # of bytes


/** Compute a hash digest of the given msg buffer.
 *
 * @param {Buffer} msg Payload to hash
 * @return {Buffer} The (blake2b) hash digest
 * @private
 */
export function digest(msg) {
  return Buffer.from(
    blake2b(DIGEST_LENGTH)
      .update(msg)
      .digest()
  );
}
