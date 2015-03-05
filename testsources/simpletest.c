int hundred(int *p) {
  int sum = 0;
  int i = 0;
  while (sum < 100) {
    sum += p[i];
    i++;
  }
  return i;
}
