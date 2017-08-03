cube = [6, 3, 5, 2, 4, 1]
starting_cube = cube[:]

solution = [1, 2, 3, 4, 5, 6]

steps = []


def rot0():
    cube[0:4] = cube[3::-1]


def rot1():
    cube[1:5] = cube[4:0:-1]


def rot2():
    cube[2:6] = cube[5:1:-1]


def main():
    global cube
    global steps

    print("The 1D-cube is at", cube)
    print("Press 0, 1 or 2 to rotate.")
    print("Type 'r' to start over.")
    print("Type 's' to show your past steps.")

    while True:
        try:
            cmd = input("{} > ".format(len(steps)))
        except EOFError:
            print()
            break

        if cmd == '0':
            rot0()
        elif cmd == '1':
            rot1()
        elif cmd == '2':
            rot2()
        elif cmd == 'r':
            cube = starting_cube[:]
            steps = []
            print("starting over:", cube)
            continue
        elif cmd == 's':
            print(steps)
            continue
        else:
            print("no clue")
            continue

        steps.append(int(cmd))

        if cube == solution:
            print("solved!")
            print(cube)
            break
        else:
            print(cube)


if __name__ == "__main__":
    main()
