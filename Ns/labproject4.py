import re
import math

def load_common_passwords():
    with open("C:\\Users\\tanis\\OneDrive\\Documents\\words.txt", "r") as file:
        return set(password.strip() for password in file)

def is_common_password(password, common_passwords):
    return password.lower() in common_passwords

def entropy(password):
    char_set = 0
    length = len(password)
    if re.search(r"\d", password):
        char_set += 10
    if re.search(r"[A-Z]", password):
        char_set += 26
    if re.search(r"[a-z]", password):
        char_set += 26
    if re.search(r"[ !\"#$%&'()*+,-./:;<=>?@[\]^_`{|}~]", password):
        char_set += 33
    
    return math.log(char_set ** length, 2)

def password_strength(password):
    common_passwords = load_common_passwords()
    score = 0
    length_weight = 2
    number_weight = 2
    uppercase_weight = 2
    lowercase_weight = 2
    special_char_weight = 3
    consecutive_chars_penalty = 2
    sequential_chars_penalty = 2
    dictionary_penalty = 5
    keyboard_pattern_penalty = 3
    
    # Check length
    score += min(len(password) // 8, 1) * length_weight
    
    # Check for numbers
    if re.search(r"\d", password):
        score += number_weight
    
    # Check for uppercase letters
    if re.search(r"[A-Z]", password):
        score += uppercase_weight
    
    # Check for lowercase letters
    if re.search(r"[a-z]", password):
        score += lowercase_weight
    
    # Check for special characters
    if re.search(r"[ !\"#$%&'()*+,-./:;<=>?@[\]^_`{|}~]", password):
        score += special_char_weight
    
    # Penalty for consecutive characters
    consecutive_char_regex = r"(\w)\1{2,}"
    if re.search(consecutive_char_regex, password):
        score -= consecutive_chars_penalty
    
    # Penalty for sequential characters
    sequential_chars_regex = r"abcdefghijklmnopqrstuvwxyz|ABCDEFGHIJKLMNOPQRSTUVWXYZ|0123456789"
    if re.search(sequential_chars_regex, password):
        score -= sequential_chars_penalty
    
    # Penalty for common passwords
    if is_common_password(password, common_passwords):
        score -= dictionary_penalty
    
    # Penalty for keyboard patterns
    keyboard_patterns = ["qwerty", "asdf", "zxcvbn", "12345"]
    for pattern in keyboard_patterns:
        if pattern in password.lower():
            score -= keyboard_pattern_penalty
    
    # Bonus for high entropy
    entropy_score = entropy(password)
    score += entropy_score
    
    return max(0, score)

def main():
    password = input("Enter your password: ")

    if password.lower() == "password" or password == "12345678":
        print("Very Weak password")
    else:
        strength = password_strength(password)
        
        if strength <= 5:
            print("Very Weak password")
        elif strength <= 10:
            print("Weak password")
        elif strength <= 15:
            print("Moderate password")
        elif strength <= 25:  # Adjusted threshold for "Very Strong password"
            print("Strong password")
        else:
            print("Very Strong password")

if __name__ == "__main__":
    main()
