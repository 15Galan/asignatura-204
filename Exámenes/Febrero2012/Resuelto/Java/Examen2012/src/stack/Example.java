	package stack;
	
	public class Example {
		static public void main(String [] args) {
			Stack<Integer> stack = new StackLink<Integer>();
			stack.push(3);
			stack.push(2);
			stack.push(1);
			System.out.println(stack);
			while (!stack.isEmpty()) {
				System.out.println(stack.top());
				stack.pop();
			}
		}
	}
