/**
 class_Individual.cpp
 Purpose: C++ class Individual.
 Details: C++ implementation.
  
 @author Mikkel Meyer Andersen
 */

/*
==========================================
Individual
==========================================
*/

#include "class_Individual.h"

Individual::Individual(int pid, 
                       int birthyear, 
                       bool surrogate, 
                       int org_paternalped_id) {
  
  m_pid = pid;
  m_birthyear = birthyear;
  m_surrogate = surrogate;
  m_org_paternalped_id = org_paternalped_id;
  
  m_father = nullptr;
}

int Individual::get_pid() const {
  return m_pid;
}

int Individual::get_birthyear() const {
  return m_birthyear;
}

bool Individual::is_surrogate() const {
  return m_surrogate;
}

int Individual::get_org_paternalped_id() const {
  return m_org_paternalped_id;
}

std::shared_ptr<Individual> Individual::get_father() const {
  return m_father;
}

int Individual::get_father_pid_safe() const {
  
  if (m_father) {
    return m_father->get_pid();
  }
  
  return -1;
}

std::vector< std::shared_ptr<Individual> > Individual::get_children() const {
  return m_children;
}

void Individual::set_father(const std::shared_ptr<Individual> father) {
  m_father = father;
}

void Individual::add_child(const std::shared_ptr<Individual> child) {
  m_children.push_back(child);
}

std::ostream& operator<<(std::ostream &os, const Individual& m) { 
  std::string father_pid = 
    (m.get_father() ? std::to_string(m.get_father()->get_pid()) : "(NA)");
  
  os << (m.is_surrogate() ? "[SURR]" : "[    ]") << ": " << 
    "pid = " << m.get_pid() << ",\t" << 
    "birthyear = " << m.get_birthyear() << ",\t" <<
    "father pid = " << father_pid << ",\t" << 
    "#children = " << m.get_children().size();
  
  return os;
}
