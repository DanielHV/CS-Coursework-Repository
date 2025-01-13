//clase Nodo para la clase Lista creada desde 0
public class Nodo<T> {
    protected T datos;
    protected Nodo<T> siguiente;
    protected Nodo<T> anterior;

    public Nodo(T datos){
        this.datos = datos;
    }
    
    public T getDatos(){
        return datos;
    }

    public void setSiguiente(Nodo<T> siguiente){
        this.siguiente = siguiente;
    }

    public void setAnterior(Nodo<T> anterior){
        this.anterior = anterior;
    }

    public Nodo<T> getSiguiente(){
        return siguiente;
    }

    public Nodo<T> getAnterior(){
        return anterior;
    }

}
