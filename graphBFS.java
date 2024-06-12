class graphBFS{
  int[] d;//distance 
  int[] p;//parents
  public static void main(String[] args){
    int s;
    int[][] graph;
  }
  private static void bfs(int[][] graph,int s,int n){
    List<Integer> nodes=new ArrayList<Integer>();
    boolean[] visited=new boolean[n];
    d=new int[n];
    p=new int[n];
    p[s]=-1;
    Queue q=new LinkedList<>();
    q.offer(s);
    visited[s]=true;
    while(!q.isEmpty()){
      int v=q.poll();
      for(int u : graph[v]){
        if(visited[u]==false){
          visited[u]=true;
          //print u
          q.offer(u);
          d[u]=d[v]+1;
          p[u]=v;
        }
    }
    
}
  
