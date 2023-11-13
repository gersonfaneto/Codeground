def main():
    iter = int(input())

    jobs = {"bonecos": 8, "arquitetos": 4, "musicos": 6, "desenhistas": 12}

    times = [0, 0, 0, 0]

    for i in range(iter):
        _, job, time = input().split()
        times[list(jobs.keys()).index(job)] += int(time)

    presents = 0

    for i in range(4):
        presents += times[i] // list(jobs.values())[i]

    print(presents)


if __name__ == "__main__":
    main()
