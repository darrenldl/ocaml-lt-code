void xor_onto (int len, const unsigned char* src, unsigned char* onto) {
  for (int i = 0; i < len; i++) {
    onto[i] ^= src[i];
  }
}
