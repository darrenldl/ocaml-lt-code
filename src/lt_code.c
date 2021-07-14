void xor_onto (int len, const char* src, char* onto) {
  for (int i = 0; i < len; i++) {
    onto[i] ^= src[i];
  }
}
