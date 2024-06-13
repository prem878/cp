import java.util.LinkedList;
import java.util.Queue;

//Single source shortest path problem
public class shortestPath {
    public static void main(String[] args){
        int[][] adj={{1,2,3},{0},{0,3,4,5},{0,5},{2,6},{2},{4}};
        System.out.println(bfs(adj,0,6));
    }
    public static int bfs(int[][] adj,int s,int d){
        int n=adj.length;
        int[] dis=new int[n];
        boolean[] visited=new boolean[n];
        dis[s]=0;
        Queue<Integer> q=new LinkedList<>();
        q.offer(s);
        visited[s]=true;
        while(!q.isEmpty()){
            int v=q.peek();
            q.poll();
            for(int u : adj[v]){
                if(!visited[u]){
                    dis[u]=dis[v]+1;
                    visited[u]=true;
                    q.offer(u);
                }
            }
        }
        return dis[d];
    }
}
