int main()
{
  Knave k;
  Knight n; 
  int a; 
  k = new_knave(10,20);
  n = new_knight(10, 20);
  a = 2; 
  knave_die(k);
  print(k.health);
  print(a); 
  return 0;

}
