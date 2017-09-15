using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace EulerProblemsCSharp
{
    static class Problem3
    {
        public static UInt64[] GetPrimes(int count)
        {
            UInt64[] primes = new UInt64[count];

            int primeIndex = 0;
            for (UInt64 i = 1; primeIndex < count; i++)
                if (IsPrime(i))
                {
                    primes[primeIndex] = i;
                    primeIndex++;
                }

            return primes;
        }

        public static bool IsPrime(UInt64 number)
        {
            //Console.WriteLine("Given number: {0}", number);

            if (number == 1 || number == 2)
                return true;

            UInt64 counter = 2;
            while (counter < number)
            {
                if ((number % counter) == 0)
                    return false;
                counter++;
            }
            return true;
        }

        public static UInt64 Solve(UInt64 endNumber, int primeCount)
        {
            //find average number of primes
            UInt64[] primes = GetPrimes(primeCount);
            Console.WriteLine("{0}th founded prime: {1}\n", primeCount, primes[primeCount - 1]);

            UInt64 divider;
            UInt64 lastPrimeFactor = 0;
            divider = 1;
            foreach (UInt64 prime in primes)
            {
                if (endNumber % prime == 0)
                {
                    divider *= prime;
                    Console.WriteLine("Prime: {0}, Divider: {1}", prime, divider);
                    if (divider == endNumber)
                    {
                        Console.WriteLine("\nLast prime factor is: {0}", prime);
                        lastPrimeFactor = prime;
                        break;
                    }
                }
            }

            return lastPrimeFactor;
        }
    }
}
