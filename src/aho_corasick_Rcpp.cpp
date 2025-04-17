#include <Rcpp.h>
#include <vector>
#include <unordered_map>
#include <queue>

using namespace Rcpp;
using namespace std;

struct TrieNode {
  unordered_map<char, TrieNode*> children;
  TrieNode* fail;
  vector<string> output;
  
  TrieNode() : fail(nullptr) {}
};

class AhoCorasick {
public:
  AhoCorasick(const vector<string>& patterns) {
    root = new TrieNode();
    buildTrie(patterns);
    buildFailLinks();
  }
  
  ~AhoCorasick() {
    deleteTrie(root);
  }
  
  vector<pair<string, int>> search(const string& text) {
    vector<pair<string, int>> matches;
    TrieNode* state = root;
    
    for (size_t i = 0; i < text.size(); ++i) {
      char ch = text[i];
      while (state != root && state->children.find(ch) == state->children.end()) {
        state = state->fail;
      }
      
      if (state->children.find(ch) != state->children.end()) {
        state = state->children[ch];
      }
      
      for (const auto& pattern : state->output) {
        matches.emplace_back(pattern, i - pattern.size() + 1);
      }
    }
    
    return matches;
  }
  
private:
  TrieNode* root;
  
  void buildTrie(const vector<string>& patterns) {
    for (const string& pattern : patterns) {
      TrieNode* current = root;
      for (char ch : pattern) {
        if (current->children.find(ch) == current->children.end()) {
          current->children[ch] = new TrieNode();
        }
        current = current->children[ch];
      }
      current->output.push_back(pattern);
    }
  }
  
  void buildFailLinks() {
    queue<TrieNode*> q;
    root->fail = root;
    
    for (auto& pair : root->children) {
      pair.second->fail = root;
      q.push(pair.second);
    }
    
    while (!q.empty()) {
      TrieNode* state = q.front();
      q.pop();
      
      for (auto& pair : state->children) {
        char ch = pair.first;
        TrieNode* child = pair.second;
        TrieNode* failState = state->fail;
        
        while (failState != root && failState->children.find(ch) == failState->children.end()) {
          failState = failState->fail;
        }
        
        if (failState->children.find(ch) != failState->children.end()) {
          child->fail = failState->children[ch];
        } else {
          child->fail = root;
        }
        
        child->output.insert(child->output.end(), child->fail->output.begin(), child->fail->output.end());
        q.push(child);
      }
    }
  }
  
  void deleteTrie(TrieNode* node) {
    for (auto& pair : node->children) {
      deleteTrie(pair.second);
    }
    delete node;
  }
};

//' Aho-Corasick multiple pattern matching algorithm using C++
//'
//' @description Searches for all occurrences of multiple patterns in a given text using the Aho-Corasick algorithm.
//' @param text A character string representing the main text.
//' @param patterns A character vector containing multiple patterns to search for.
//' @return A list of integers indicating the starting positions of each match (1-based index).
// [[Rcpp::export]]
List aho_corasick_Rcpp(std::string text, std::vector<std::string> patterns) {
  AhoCorasick ac(patterns);
  vector<pair<string, int>> matches = ac.search(text);
  
  unordered_map<string, vector<int>> grouped_results;
  for (const auto& match : matches) {
    grouped_results[match.first].push_back(match.second + 1);
  }
  
  List result;
  for (const auto& entry : grouped_results) {
    result.push_back(List::create(Named("pattern") = entry.first, Named("positions") = entry.second));
  }
  
  return result;
}
