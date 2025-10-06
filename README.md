# Gcalc

A multi-stack terminal based scientific RPN calculator with functions specifically tailored to physics applications.

## Core principles of operation
The Reverse Polish Notation (RPN) system works traditionally by storing values to perform calculations on in so called registers. The operation to be performed on the contents of registers is specified after all required input values have been placed in registers. With the specific register a value is stored in denoting its role in a calculation to be performed.
In a traditional RPN calculator registers would usually be identified by letters. Such that there would be the X, Y and Z register for instance.
The registers are then filled with inputs in order with each input value pushing the others up the stack and each operation on the registers consuming and thus removing its inputs from the stack, making room for the result or results.

### Example 
In a traditional RPN calculator the calculation 1+2=? would be performed by first entering 1 placing it in the x register.

| Register | Value |
|----------|-------|
| Z        |  0    |
| Y        |  0    |
| X        |  1    |

Next the 2 is entered placing it into the x register and pushing the 1 up into the y register.

| Register | Value |
|----------|-------|
| Z        |  0    |
| Y        |  1    |
| X        |  2    |

Now the operation is specified by entering +. The two operands in the X and Y register are now added together as specified by the + operator and the result is stored in the X register.

| Register | Value |
|----------|-------|
| Z        |  0    |
| Y        |  0    |
| X        |  3    |

## Expanding upon a basic RPN calculator. 

Gcalc seeks to take advantage of the impressive memory available on home computers by allowing an arbitrary number of registers, only limited by available memory to be allocated and populated with values for use in calculations. 
To facilitate this registers are no longer referred to by letters but instead by numbers. Thus X becomes r0, Y becomes r1, Z becomes r2 and registers past that are referred to as r3, r4, r5 and so on.

In an effort to tailor gcalc for its intended use case (Getting me through a physics degree without having to rely upon a fully featured computer algebra system) Gcalc supports both the allocation of multiple stacks so that long calculations can be organized without external organization aids such as paper. 
Stacks can themselves be used as inputs for either stack wide operations such as __scale__ or in stack on stack operations such as __sadd__.

## Supported commands
| Command Mnemonic | Command Name             | Command Action                                                                                                                                                                                                                                                                                               |
|------------------|--------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| +                | Add                      | Add the first two numbers on the stack (r0+r1)                                                                                                                                                                                                                                                               |
| -                | Subtract                 | Subtract the first two numbers on the stack (r0=r1-r0)                                                                                                                                                                                                                                                       |
| *                | Multiply                 | Multiply the first two numbers on the stack (r0=r1*r0)                                                                                                                                                                                                                                                       |
| /                | Divide                   | Divide the value second lowest on the stack by the lowest value on the stack. (r0=r1/r0)                                                                                                                                                                                                                     |
| n                | Change sign              | Changes the sign of the value lowest on the stack. (r0=-r0)                                                                                                                                                                                                                                                  |
| swp              | Swap                     | Swaps the positions of the values lowest and second lowest of the stack. (r0=r1, r1=r0)                                                                                                                                                                                                                      |
| rol              | Roll Stack               | Moves all values on the stack one position lower on the stack. The value in r0 is moved to the top of the stack. (r0=r1, r1=r2, r2=r3... r_n=r_n+1)                                                                                                                                                          |
| dup              | Duplicate                | Duplicate the value in r0 one position up the stack. Rolls the rest of the stack upwards. (r1=r0, r2=r1, r3=r2, r_n=r_n-1)                                                                                                                                                                                   |
| pow              | Power / Exponent         | Multiply the value of r0 with itself r1 times. (r0=r0^r1)                                                                                                                                                                                                                                                    |
| sqrt             | Square root              | Replace the value in r0 with its square root. (r0=sqrt(r0))                                                                                                                                                                                                                                                  |
| rt               | Root                     | Replace the value in r0 with its r1th root. (r0=rt(r0,r1))                                                                                                                                                                                                                                                   |
| product          | Stack product            | Take the product of all values on the stack and stores it in r0, collapsing the stack.                                                                                                                                                                                                                       |
| scale            | Scale stack              | Multiply all values positioned higher than r0 on the stack by r0. Removes r0 from the stack.(r1=r1*r2,r2=r2*r0,r_n=r_n*r0)                                                                                                                                                                                   |
| det              | Determinant              | Takes the Determinant of a matrix formed by values higher than r0 on the stack. r0 specifies the number of columns in the Matrix.                                                                                                                                                                            |
| sum              | Stack Sum                | Add all values on the stack and store the result in r0, collapsing the stack. (r0=r0+r1+r2+r3... r_n)                                                                                                                                                                                                        |
| d                | Delete                   | Remove value in r0 from the stack and rolls the stack downwards. (r0=r1,r1=r2,... r_n=r_n+1)                                                                                                                                                                                                                 |
| cls              | Clear stack              | Remove all values on the current stack.                                                                                                                                                                                                                                                                      |
| p                | print                    | Print the current stack to stdout.                                                                                                                                                                                                                                                                           |
| switch           | Switch Stack             | Switches the current stack to the stack with the id specified in r0                                                                                                                                                                                                                                          |
| merge            | Merge Stacks             | Push all values of the stack with id specified in r0 onto the current stack. Consumes value in r0 but neither of the stacks.                                                                                                                                                                                 |
| sadd             | Add Stack to Stack       | Add values of stack specified by id in r0 to the values in current stack according to their positions, consuming the stack id in r0 (r0=r1+s(id).r0, r1=r2+s(id).r1, r2=r3+s(id).r2 ... r_n=r_(n+1)+s(id).r_n)                                                                                               |
| sproduct         | Multiply Stack by Stack  | Multiply the values of stack specified by id in r0 with the values in the current stack according to their positions. The resultants are stored in the corresponding positions in the current stack. The stack id in r0 is consumed. (r0=r1*s(id).r0, r1=r2*s(id).r1, r2=r3*s(id).r2, r_n=r_(n+1)*s(id).r_n) |
| sdiv             | Divide Stack by Stack    | Divide the values of stack specified by id in r0 with the values in the current stack according to their positions. The resultants are stored in the corresponding positions in the current stack. The stack id in r0 is consumed (r0=r1/s(id).r0, r1=r2/s(id).r1, r2=r3/s(id).r2 ... r_n=r_(n+1)/s(id).r_n) |
| scross           | Stack cross product      | Form a vector of the first 3 values of the current stack and the stack identified by the stack id in r0 respectively, after the stack id has been consumed. Compute the cross product of the two vectors and store the components on the current stack.                                                      |

More commands will be added as soon as they are required.
