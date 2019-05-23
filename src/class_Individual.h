/**
 class_Individual.h
 Purpose: Header for C++ class Individual.
 Details: C++ header.
  
 @author Mikkel Meyer Andersen
 */

#ifndef CLASS_INDIVIDUAL_H
#define CLASS_INDIVIDUAL_H

// [[Rcpp::plugins(cpp17)]]
#include <Rcpp.h>

#include <vector>
#include <memory> // shared_ptr + make_unique
#include <iostream>

class Individual {
private:
  int m_pid; 
  int m_birthyear;
  bool m_surrogate;
  
  std::shared_ptr<Individual> m_father;
  std::vector< std::shared_ptr<Individual> > m_children;
  
public:
  Individual(int pid, int birthyear, bool surrogate);
  
  int get_pid() const;
  int get_birthyear() const;
  bool is_surrogate() const;
  
  std::shared_ptr<Individual> get_father() const;
  std::vector< std::shared_ptr<Individual> > get_children() const;
  
  void set_father(const std::shared_ptr<Individual> father);
  void add_child(const std::shared_ptr<Individual> child);
  
  ////////////////////////////////////////////////////////////  
  
  friend std::ostream& operator<<(std::ostream &out, const Individual &m);
  
};

#endif
