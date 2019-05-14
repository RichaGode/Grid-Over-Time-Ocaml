int main()
{
  Grid g;
  Knave k;
  int b;
  int a;
  

  k = new_knave(30,20);
  g = grid_init(20,50);
  print(get_knave_x_pos(k) );
  
  if (get_knave_x_pos(k)>get_grid_x(g) ){

    print(get_grid_x(g));
    print(get_knave_x_pos(k));

    a = get_grid_x(g) - get_knave_x_pos(k);
    print(a);
    k = move_knave(k, a, 0);
  }
  
  b = get_knave_x_pos(k);
  print(b);

  return 0; 
}