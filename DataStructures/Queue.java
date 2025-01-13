//subclase de la clase Lista creada desde 0
public class Queue<T> extends Lista{
    public Queue(){
        super();
    }

    public void imprimeQueue(){
        Nodo<T> actual = this.primerNodo;
        if(this.primerNodo == null){
            System.out.println("La lista esta vacia");
        }else{
            System.out.print("Inicio de la cola --)   ");
            while(actual != null){
                if(actual.getSiguiente() == this.primerNodo){
                    System.out.println(actual.getDatos() + "   (-- Fin de la cola");
                    break;
                }else{
                    System.out.print(actual.getDatos() + " <- ");
                    actual = actual.getSiguiente();
                }
                
            }
        }
    }

    public void encolar(T objeto){
        this.insertarAlFinal();
    }

    public void desencolar(){
        this.eliminarByIndice(this.getTamanio()-1);
    }

    public void insertarByIndice(){
        System.out.println("No se puede usar este metodo en una cola");
    }

    public void insertarAlFinal(){
        System.out.println("No se puede usar este metodo en una cola");
    }
}
