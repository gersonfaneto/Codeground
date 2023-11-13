from math import pow
from sys import stdin

def main():
    for line in stdin:
        line = line.rstrip()
        sum = 0; j = 0
        for i in range(len(line) - 1, -1, -1):
            sum += (ord(line[i]) - 64) * (int (pow(26, j)))
            j += 1

        if sum <= 16384:
            print(sum)
        else:
            print("Essa coluna nao existe Tobias!")


if __name__ == "__main__":
    main()
