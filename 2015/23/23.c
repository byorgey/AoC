int main() {
  int a = 4591;
  int b = 0;
  while (a != 1) {
    b++;
    if (a % 2 == 0) {
      a = a / 2;
    } else {
      a = a*3 + 1;
    }
  }
  return b;
}

