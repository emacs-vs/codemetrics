import "fmt"

func main() {
	v1 := false
	v2 := true
	
	for {
		if v1 {
			fmt.Println(v1)
		}

		if v2 {
			fmt.Println(v2)
		}
	}

	for {}
}
