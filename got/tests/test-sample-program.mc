int main()
{
  Knave k;
  Knight n; 
  k = new_knave(10, 20);
  n = new_knight(10, 20);

  while (k.health > 0) {
    n = attack_knight(k, n);
    print(n.health);
    k = attack_knave(n,k);
    print(k.health);
  }
  if (get_knight_health(n) <= 0){
    knight_die(n);
  }

  if (get_knave_health(k) <= 0){
    knave_die(k);
  }

  return 0;
}



initalize all objects

Grid g;
Knight k;
Knave j;
g = grid_init(100, 100);

k = new_knight()

print(grid.max_time);




