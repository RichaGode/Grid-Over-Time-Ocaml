int main()
{
  Grid g;
  int i;
  i=2;
  g = grid_init(10,20);
  
  print(g.max_time);
  
  g = set_max_time(g, 20);
  print(get_max_time(g));

  while (i >0) {
    i = i-1;
    print(get_current_time(g) );
    g = set_current_time(g, (g.current_time+10));
  }

  print(g.current_time);
  return 0; 
}