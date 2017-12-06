with open('day05.txt') as f:
    mem = []
    for line in f:
        mem.append(int(line.rstrip()))
    print mem

    index = 0;
    counter = 0;
    while index >= 0 and index < len(mem):
        newIndex = index + mem[index]
        if mem[index] >= 3:
            mem[index] = mem[index] -1
        else:
            mem[index] = mem[index] + 1
        index = newIndex
        counter = counter + 1
    print counter
