public class Stack<T> extends Lista{
    public Stack(){
        super();
    }

    public void imprimeStack(){
        Nodo<T> actual = this.primerNodo;
        if(this.primerNodo == null){
            System.out.println("La lista esta vacia");
        }else{
            System.out.print("Tope --)   ");
            while(actual != null){
                if(actual.getSiguiente() == this.primerNodo){
                    System.out.println(actual.getDatos() + "   (-- Fondo");
                    break;
                }else{
                    System.out.print(actual.getDatos() + " <- ");
                    actual = actual.getSiguiente();
                }
                
            }
        }
    }

    public void pop(){
        this.eliminarByIndice(0);
    }

    public T getTope(){
        return (T) this.getPrimerElemento();
    }

}
