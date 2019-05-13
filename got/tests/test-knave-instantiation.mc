int main()
{
  Knave k;
  int a; 
  k = new_knave(10,20);
  a = get_stealth(k);
  print(a);
  k = set_stealth(k, 0);
  a = get_stealth(k); 
  print(a);
  return 0; 
}
