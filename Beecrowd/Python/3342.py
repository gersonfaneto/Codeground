from math import floor

def main():
    size = int(input())

    blacks = floor((size * size) / 2)
    whites = (size * size) - blacks

    print(f"{whites} casas brancas e {blacks} casas pretas")


if __name__ == "__main__":
    main()
