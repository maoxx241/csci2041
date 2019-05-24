public class Uncurried{
  public static int pow(int base, int exp){
    int ans = 1;
    for(int i=1; i<exp; i++){
      ans = ans * base;
    }
    return ans;
  }

  public static void print_powers(int base, int start, int stop){
    for(int i=start; i<stop; i++){
      int x = pow(base);
      System.out.printf("%d^%d is %d\n",base,i,x);
    }
  }

  public static void main(String args[]){
    if(args.length < 3){
      System.out.printf("usage: java Uncurried base start stop\n");
      System.exit(1);
    }
    int base  = Integer.parseInt(args[0]);
    int start = Integer.parseInt(args[1]);
    int stop  = Integer.parseInt(args[2]);
    print_powers(base,start,stop);
  }
}
        
