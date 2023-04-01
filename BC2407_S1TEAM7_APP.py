import streamlit as st
import pandas as pd
from sklearn.ensemble import RandomForestClassifier
import random
import PIL
import streamlit_ace as st_ace
import time as time
from PIL import Image
import requests



# Set random seed
random.seed(42)

# Load dataset
data_url="https://raw.githubusercontent.com/jaijindal/BC2407_S1TEAM7_ATTRITION_APP/main/Final_Dataset.csv"
data = pd.read_csv(data_url)

# Split dataset into features and target variable
X = data.drop('Attrition', axis=1)
y = data['Attrition'] 

# Initialize Random Forest Classifier
rf = RandomForestClassifier(n_estimators=500, random_state=42)
rf.fit(X, y)

# Define main function
def main():
    # Set page title and layout
    st.set_page_config(page_title="Coinbase HR Web App", layout="wide")

    # Define sidebar
    st.sidebar.title("Navigation")
    tabs = ["Home Page","Attrition Prediction"]
    selected_tab = st.sidebar.radio("Select a tab", tabs)

    # Render the selected page
    if selected_tab=="Home Page":
         show_home_page()   
    elif selected_tab == "Attrition Prediction":
        show_prediction()



def show_home_page():


    st.write(f"<h1 style='text-align: center; font-size: 3em;'>Coinbase WebApp For Human Resources</h1>", unsafe_allow_html=True)
    image1_url="https://raw.githubusercontent.com/jaijindal/BC2407_S1TEAM7_ATTRITION_APP/main/coinbase_logo3.png"
    image1 = Image.open(requests.get(image1_url, stream=True).raw)
    #image1=PIL.Image.open(image1_url)
    new_size = (1200, 700)
    image1 = image1.resize(new_size)
    col1, col2, col3 = st.columns([1,6,1])

    with col1:
        st.write("")

    with col2:
        st.image(image1)
        st.write()
        st.write("        (Click on Navigation Sidebar to begin)      ")

    with col3:
        st.write("")
        
    #st.write("This app provides recommendations to improve employee retention and predict attrition probability at Coinbase.")




# Define function to show attrition prediction form
def show_prediction():
    st.title("Attrition Prediction")
    


    names = ['Amelia', 'Benjamin', 'Charlotte', 'Dylan', 'Ella', 'Fiona', 'Grace', 'Henry', 'Isabella']
    selected_option = st.selectbox("Select the Coinbase Employee whose Attrition you want to predict:", names, index=1, key="myselectbox")
    
    if selected_option=="Amelia":
        a=0
        b=1
        c=0
        d=3
        e=1
        f=3
        g=1
        h=3
        i=0
        j=10
        k=8
        l=3
        m=11000
        image5_url="https://raw.githubusercontent.com/jaijindal/BC2407_S1TEAM7_ATTRITION_APP/main/headshot2.jpg"

       
    if selected_option=="Benjamin":
        a=0
        b=1
        c=1
        d=2
        e=1
        f=3
        g=0
        h=1
        i=0
        j=1
        k=0
        l=4
        m=2000
        image5_url="https://raw.githubusercontent.com/jaijindal/BC2407_S1TEAM7_ATTRITION_APP/main/headshot1.jpg"


    if selected_option=="Charlotte":
        a=3
        b=2
        c=0
        d=2
        e=0
        f=3
        g=2
        h=4
        i=1
        j=6
        k=3
        l=4
        m=10000
        image5_url="https://raw.githubusercontent.com/jaijindal/BC2407_S1TEAM7_ATTRITION_APP/main/headshot3.jpg"

        
    if selected_option=="Dylan":
        a=2
        b=1
        c=1
        d=3
        e=1
        f=3
        g=2
        h=2
        i=1
        j=6
        k=0
        l=3
        m=6000
        image5_url="https://raw.githubusercontent.com/jaijindal/BC2407_S1TEAM7_ATTRITION_APP/main/headshot4.jpg"

    
   
    if selected_option=="Ella":
        a=1
        b=1
        c=0
        d=3
        e=1
        f=2
        g=2
        h=3
        i=0
        j=10
        k=9
        l=2
        m=6000
        image5_url="https://raw.githubusercontent.com/jaijindal/BC2407_S1TEAM7_ATTRITION_APP/main/headshot5.jpg"


    if selected_option=="Fiona":
        a=4
        b=1
        c=0
        d=2
        e=1
        f=4
        g=1
        h=4
        i=15
        j=40
        k=6
        l=4
        m=10000
        image5_url="https://raw.githubusercontent.com/jaijindal/BC2407_S1TEAM7_ATTRITION_APP/main/headshot6.jpg"



    if selected_option=="Grace":
        a=1
        b=1
        c=0
        d=3
        e=2
        f=1
        g=0
        h=1
        i=1
        j=7
        k=7
        l=4
        m=5000
        image5_url="https://raw.githubusercontent.com/jaijindal/BC2407_S1TEAM7_ATTRITION_APP/main/headshot7.jpg"

        
    if selected_option=="Henry":
        a=0
        b=1
        c=1
        d=1
        e=2
        f=3
        g=0
        h=3
        i=0
        j=1
        k=0
        l=4
        m=2000
        image5_url="https://raw.githubusercontent.com/jaijindal/BC2407_S1TEAM7_ATTRITION_APP/main/headshot8.jpg"

       
    if selected_option=="Isabella":
        a=0
        b=1
        c=0
        d=2
        e=2
        f=4
        g=0
        h=3
        i=12
        j=20
        k=7
        l=4
        m=17000
        image5_url="https://raw.githubusercontent.com/jaijindal/BC2407_S1TEAM7_ATTRITION_APP/main/headshot9.jpg"

        
    col1, col2, col3 = st.columns([2,2,2])
    new_size = (300, 300)
    image5 = Image.open(requests.get(image5_url, stream=True).raw)

    image5 = image5.resize(new_size)


        
    with col1:
        st.write()

    with col2:
        st.image(image5)

    with col3:
        st.write()
        
        
        
        
    st.write(" ")
    st.write(" ")
    st.write(" ")
    st.write(" ")
    st.write("These are the variables which affect if " +selected_option+ " will stay at Coinbase or leave.")
    st.write('''These variables have the default values set as the current data of ''' +selected_option+ '''. Human Resources are
             encouraged to toggle these variables to try out different combinations that would best suit ''' +selected_option+ ''' 
             and enable retention if ''' +selected_option+ ''' is at risk of leaving. Human Resources are also encouraged to take 
             personalized recommendations for the employees (if they are at risk of leaving) printed at the bottom of the screen as a framework. ''')
    st.write(" ")
    st.write(" ")
    

    # Create input form for the user
    job_role = st.slider('Job Role',min_value=0, max_value=6, step=1 , value=a)
    business_travel = st.slider('Business Travel', min_value=0, max_value=2, step=1,value=b)
    overtime = st.slider('Overtime', min_value=0, max_value=1, step=1,value=c)
    work_life_balance = st.slider('Work Life Balance', min_value=1, max_value=4, step=1, value=d)
    marital_status = st.slider('Marital Status', min_value=0, max_value=2, step=1 , value=e)
    environment_satisfaction = st.slider('Environment Satisfaction', min_value=1, max_value=4, step=1 , value=f)
    stock_option_level = st.slider('Stock Option Level', min_value=0, max_value=3, step=1, value=g)
    education = st.slider('Education ', min_value=1, max_value=5, step=1, value=h)
    years_since_last_promotion = st.slider('Years Since Last Promotion', min_value=0, max_value=15, step=1 , value=i)
    years_at_company = st.slider('Years at Company', min_value=0, max_value=40, step=1 , value=j)
    years_with_curr_manager = st.slider('Years with Current Manager', min_value=0, max_value=17, step=1 , value=k)
    job_satisfaction = st.slider('Job Satisfaction', min_value=1, max_value=4, step=1 , value=l)
    monthly_income = st.slider('Monthly Income', min_value=1000, max_value=15000, step=1000 , value=m)
    

    
    
    # Add a "Predict" button
    if st.button('Predict'):
        # Make predictions using the Random Forest Classifier
        prediction = rf.predict([[job_role, business_travel, overtime, work_life_balance, marital_status, 
                                   environment_satisfaction, stock_option_level, education, years_since_last_promotion,
                                   years_at_company, years_with_curr_manager, job_satisfaction, monthly_income]])
        # Display the prediction to the user
        if prediction[0] == 0:
            st.write('### **The employee is NOT LIKELY to leave.**')
        else:
      
            st.write('### **The employee is LIKELY to leave.**')
            # Provide recommendations to improve attrition
            
            st.write(" ")

            if business_travel == 2:
                st.write('The employee is likely to leave. To improve attrition, consider reducing business travel time for the employee for the next 6 months.')
            
            if environment_satisfaction <=3:
                st.write("The employee is not receiving enough opportunities to learn and grow, they may become disengaged and are likely to leave. To improve attrition, consider offering training programs, mentoring, or career development opportunities.")
            
            if work_life_balance<=3:
                st.write("The employee is consistently working long hours or struggling to balance work and personal commitments, they are at higher risk for leaving. To improve attrition, consider offering flexible work arrangements, such as telecommuting or flexible schedules, to help employee better manage their time.")
            
            if monthly_income <= 3000:
                st.write('The employee might feel that their salary or benefits package is not competitive, they are likely to leave for a better offer elsewhere. To improve attrition, consider conducting a salary and benefits analysis to ensure that your compensation package is competitive within your industry and location.')
            
            if years_with_curr_manager <= 1:
                st.write('The employee is likely to leave. To improve attrition, consider assigning a manager who can offer more support and career development opportunities.')
            
            if job_satisfaction <=3:
                st.write("The employee might feel undervalued or unappreciated, and might be more disengaged and is likely to leave. To improve attrition, consider implementing recognition and reward programs that acknowledge employee contributions and accomplishments.")
            
            if years_since_last_promotion>=3:
                st.write("The employee feels that there are few opportunities for advancement within the company, they are likely to leave for a position with more growth potential. To improve attrition, consider creating a clear career path for employees and offering opportunities for internal promotion and advancement.")
            
            else:
                st.write('The employee is likely to leave. To improve attrition, consider discussing their concerns and needs, and offering support or resources to address them.')
   
            





    st.write(" ")
    st.write(" ")   
    st.write(" ")
    st.write(" ")    
    st.write(" ")
    st.write(" ")    
    st.write(" ")    
    st.write(" ")
    #st.write(" ")        
            
    col1, col2, col3 = st.columns([2,2,2])
    
    image2_url="https://raw.githubusercontent.com/jaijindal/BC2407_S1TEAM7_ATTRITION_APP/main/Index_1.png"
    image2 = Image.open(requests.get(image2_url, stream=True).raw)
    new_size = (700, 700)
    image2 = image2.resize(new_size)

    image3_url="https://raw.githubusercontent.com/jaijindal/BC2407_S1TEAM7_ATTRITION_APP/main/Index_2.png"
    image3 = Image.open(requests.get(image3_url, stream=True).raw)
    new_size = (700, 700)
    image3 = image3.resize(new_size)
    
    
    image4_url="https://raw.githubusercontent.com/jaijindal/BC2407_S1TEAM7_ATTRITION_APP/main/Index_3.png"
    image4 = Image.open(requests.get(image4_url, stream=True).raw)
    new_size = (700, 700)
    image4 = image4.resize(new_size)
    
    with col1:
        st.image(image2)

    with col2:
        st.image(image3)

    with col3:
       st.image(image4)
    


if __name__ == "__main__":
    main()

