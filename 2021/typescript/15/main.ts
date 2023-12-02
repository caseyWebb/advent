import { promises as fs } from 'fs'
import path from 'path'

main()

async function main() {
  const graph = parseInput(await getInput())
  const result = dijkstra(graph)
  console.log(result)
}

async function getInput() {
  return await fs.readFile(path.join(__dirname, 'input.txt'), 'utf8')
}

function parseInput(str: string): number[][] {
  return str
    .split('\n')
    .filter((line) => line !== '')
    .map((line) => line.split('').map((n) => parseInt(n)))
}

function dijkstra(graph: number[][]) {
  const height = graph.length * 5
  const width = graph[0].length * 5
  const unvisited = new Map<number, Set<number>>(
    range(width, (x) => [x, new Set<number>(range(height, (y) => y))])
  )
  const seen = new Map<number, Map<number, number>>(
    range(width, (x) => [x, new Map<number, number>()])
  )

  const getRisk = (x: number, y: number) => {
    let v =
      graph[y % graph.length][x % graph[0].length] +
      Math.floor(y / graph.length) +
      Math.floor(x / graph[0].length)
    if (v >= 10) {
      v = (v % 10) + 1
    }
    return v
  }

  const next = (): [number, [number, number]] =>
    Array.from(seen.entries())
      .flatMap(([x, row]) =>
        Array.from(row.entries()).map(([y, v]) => [x, y, v])
      )
      .reduce(
        ([min, minCoords], [x, y, v]) =>
          v < min ? [v, [x, y]] : [min, minCoords],
        [Infinity, [Infinity, Infinity]]
      )

  let current: [number, [number, number]] = [0, [0, 0]]

  while (unvisited.size > 0) {
    const [v, [x, y]] = current
    if (x === width - 1 && y === height - 1) {
      return v
    }
    for (const [nx, ny] of [
      [x - 1, y],
      [x + 1, y],
      [x, y - 1],
      [x, y + 1],
    ]) {
      const isUnvisited = unvisited.get(nx)?.has(ny)
      const nv = seen.get(nx)?.get(ny) || Infinity
      if (isUnvisited && v + getRisk(nx, ny) < nv) {
        seen.get(nx)?.set(ny, v + getRisk(nx, ny))
      }
    }
    unvisited.get(x)?.delete(y)
    seen.get(x)?.delete(y)
    if (seen.get(x)?.size === 0) {
      seen.delete(x)
    }
    current = next()
    if (current[0] === Infinity) {
      throw new Error('no path')
    }
  }
}

function range<T>(n: number, f: (i: number) => T): T[] {
  const arr: T[] = []
  for (let i = 0; i < n; i++) {
    arr.push(f(i))
  }
  return arr
}
