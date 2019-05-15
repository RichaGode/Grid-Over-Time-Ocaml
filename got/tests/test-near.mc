int main()
{
  Knight a;
  Knave b;

  Knight c;
  Knave d;

  Knight e;
  Knave f;

  Knight g;
  Knave h;

  Knight i;
  Knave j;

  a = new_knight(12,20);
  b = new_knave(10,40);

  if (near(a,b,2) == 1){
    print_str("X knight check");
  }

  c = new_knight(10,20);
  d = new_knave(12,40);

  if (near(c,d,2) == 1){
    print_str("X knave check");
  }

  e = new_knight(20,24);
  f = new_knave(42,20);

  if (near(e,f,5) == 1){
    print_str("Y knight check");
  }

  g = new_knight(20,20);
  h = new_knave(42,24);

  if (near(g,h,5) == 1){
    print_str("Y knave check");
  }

  i = new_knight(20,24);
  j = new_knave(42,20);

  if (near(i,j,3) == 0){
    print_str("Not close!");
  }

  return 0;
}