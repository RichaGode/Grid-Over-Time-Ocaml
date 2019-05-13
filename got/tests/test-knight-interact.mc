int main()
{
  Knave k;
  Knight n; 
  k = new_knave(10,20);
  n = new_knight(10, 20);
  while (get_knight_health(n) > 0) {
    n = attack_knight(k, n);
    print(get_knight_health(n));
  }
  if (get_knight_health(n) == 0){
    knight_die(n);
  }

  return 0;

}
