const graph: number[][] = [
    [1, 2],    // 0
    [3, 4],    // 1
    [5],       // 2
    [],        // 3
    [5],       // 4
    []         // 5
];

const queue: number[] = [];
let head = 0;  // Points to the current dequeue position
let tail = 0;  // Points to the next enqueue position

const startNode = 0;
queue[tail] = startNode;
tail++;
const visited: boolean[] = [false, false, false, false, false, false];
visited[startNode] = true;

while (head != tail) {  // While the queue is not empty
    const currentNode = queue[head];
    head++;  // Increment head to remove the current node from the queue

    // Process node

    // Check each neighbor of the current node
    const neighbors = graph[currentNode];
    for (let i = 0; i < neighbors.length; i++) {
        const neighbor = neighbors[i];
        if (!visited[neighbor]) {
            visited[neighbor] = true;
            queue[tail] = neighbor;
            tail++;
        }
    }
}
