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
int time_step; 

g = grid_init(100, 100);
g = set_max_time(g, 50); 
print(g.max_time); 

k = new_knight(random(g.x),random(g.y));
j = new_knave(random(g.x), random(g.y)); 

time_step = 2; 
while (g.current_time < g.max_time) {
  if (near(k,j,3) == 1) {

    /# if the stealth of the knave is 1, do nothing. #/

    if (j.stealth == 0) {
    /# set the stealth of the knave to be 1 #/
      print(j.health);  
      j = j.set_stealth(j, 1); 
      j = attack_knave(k,j);
      print(j.health);  
      while (k.health > 0) {
        k = attack_knight(k,j); 
      } 
    }
  }
  else {
    
  }
  
  g = set_current_time(g, (g.current_time + time_step)); 
}

once the time has been hit, make sure to kill off the grid 

// have objects moving around the grid using the move function,
if they're within 5 of each other they can interact i.e. they can both attack each other or if one has stealth then it can't be attacked (don't call it)

k = new_knight()

print(grid.max_time);




