int main()
{
  Grid g;
  Knight k;
  int x;
  int y; 
  int a;
  

  k = new_knight(30,20);
  g = grid_init(100,100);
  print(k.x);
  
  if (g.x>k.x){

    print(g.x);
    print(k.x);

    a = g.x - k.x;
    print(a);
    k = move_knight(k, g, 200, -10);
  }
  
  x = k.x;
  print(x);
  y = k.y;
  print(y); 

  return 0; 
}