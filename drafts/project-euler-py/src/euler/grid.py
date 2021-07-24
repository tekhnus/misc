def get_bounds(grid):
    return len(grid), len(grid[0])


def iter_positions(grid):
    maxi, maxj = get_bounds(grid)
    for i in range(maxi):
        for j in range(maxj):
            yield i, j


def make_slice(position, direction, length):
    for _ in range(length):
        yield position
        i, j = position
        di, dj = direction
        position = i + di, j + dj


def get(grid, slice_):
    maxi, maxj = get_bounds(grid)
    res = []
    for i, j in slice_:
        if not (0 <= i < maxi) or not (0 <= j < maxj):
            return None
        res.append(grid[i][j])
    return res
