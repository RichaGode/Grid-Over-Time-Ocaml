int main () {
  Grid g;
  Knight k;
  Knave j;
  int time_step; 

  g = grid_init(50, 50);
  g = set_max_time(g, 10); 
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
        j = set_stealth(j, 1); 
        j = attack_knave(k,j);
        print(j.health);  
        while (k.health > 0) {
          k = attack_knight(j,k); 
        } 
      }
    }
    else {
      /# now move them closer #/
      k = move_knight(k,g,10,4); 
      print(k.x);
      print(k.y); 
    }
    print(g.current_time); 
    g = set_current_time(g, (g.current_time + time_step)); 
  }
  grid_end(g); 
  knight_die(k);
  knave_die(j); 

  return 0; 

}



