int main()
{
  Knave k;
  Knight n; 
  k = new_knave(10, 20);
  n = new_knight(10, 20);
  print(n.attack_strength);
  
  k = attack_knave(n,k);
  print(k.health);
  
  n = set_knight_attack(n, 10);
  print(n.attack_strength);

  k = attack_knave(n,k);
  print(k.health);

  return 0;
}
